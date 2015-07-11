#!/usr/bin/env stack
-- stack --resolver=lts-2.16 runghc --package=stm --package=lens --package=random --package=async
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , TypeSynonymInstances
           , FlexibleInstances #-}
module Main where

--import Control.Applicative
import Control.Monad
import Control.Lens
--import Control.Lens.TH
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
--import Data.Time
import Data.Time.Clock.POSIX
import System.Random
import System.IO


{-
 - オークション
 - bid時にChargeを減らす
 - オークションを終わらせる
 -
 - Property
 -   * チャージ合計一貫性
 -   * ユーザの所持金は0以上
 -}

type UserName = String
type Price = Int

data User = User
    { _userName :: UserName
    , _userCharge :: TVar Int
    }
data Auction = Auction
    { _auctionStartTime :: POSIXTime
    , _auctionEndTime :: POSIXTime
    , _auctionPrice :: TVar Int
    , _auctionTopUser :: TVar (Maybe User)
    , _auctionStatus :: TVar AuctionStatus
    }
data AuctionConfig = AuctionConfig
    { _auctionConfigStartTime :: POSIXTime
    , _auctionConfigEndTime :: POSIXTime
    , _auctionConfigFirstPrice :: Int
    }
data AuctionStatus
    = Wait
    | Hold
    | Finish

makeFields ''User
makeFields ''Auction
makeFields ''AuctionConfig

newUser :: UserName -> Int -> STM User
newUser nm chg = do
    ch <- newTVar chg
    return $ User nm ch

newAuction :: AuctionConfig -> STM Auction
newAuction config = do
    cprice <- newTVar $ config ^. firstPrice
    emp <- newTVar Nothing
    st <- newTVar Wait

    return $ Auction
                (config ^. startTime)
                (config ^. endTime)
                cprice
                emp
                st

bid :: Auction -> User -> Price -> STM Bool
bid auction user bidPrice = do
    st <- readTVar $ auction ^. status
    case st of
        Wait -> return False
        Finish -> return False
        Hold -> do
            curPrice <- readTVar $ auction ^. price
            curCharge <- readTVar $ user ^. charge
            if curPrice < bidPrice && bidPrice <= curCharge
                then do
                    addUserCharge user (negate bidPrice)
                    -- Return previous charge to previous top user
                    mUser <- readTVar (auction ^. topUser)
                    case mUser of
                        Just preUser -> addUserCharge preUser curPrice
                        Nothing -> return ()

                    writeTVar (auction ^. price) bidPrice
                    writeTVar (auction ^. topUser) (Just user)
                    return True
                else return False

-- | Price can be negative.
addUserCharge :: User -> Price -> STM ()
addUserCharge user diff = do
    curCharge <- readTVar (user ^. charge)
    writeTVar (user ^. charge) (curCharge + diff)

startAuction :: Auction -> IO ()
startAuction auction = atomically $ writeTVar (auction ^. status) Hold

finishAuction :: Auction -> IO ()
finishAuction auction = atomically $ writeTVar (auction ^. status) Finish

say = putStrLn

--------------------------------------------------------------------------------

userThread :: User -> Auction -> IO ()
userThread user auction = do
    forever $ do
        pr <- randomRIO (100, 10000)
        success <- atomically $ bid auction user pr
        case success of
            True -> do
                say $ concat
                    [ user ^. name
                    , " bid at "
                    , show pr
                    , " and succeeded."
                    ]
                threadDelay $ 10 * 1000
            False -> do
--                say $ concat
--                    [ user ^. name
--                    , " bid at "
--                    , show pr
--                    , " and Failed."
--                    ]
                threadDelay $ 5 * 1000

checkTotal :: [User] -> Auction -> Price -> IO Bool
checkTotal users auction expected = do
    cs <- forM users $ \user -> atomically $ readTVar (user ^. charge)
    curPrice <- atomically $ readTVar (auction ^. price)
    return $ expected == curPrice + sum cs

summary :: [User] -> Auction -> Price -> IO ()
summary users auction expected = do
    forM_ users $ \ user -> do
        ch <- atomically $ readTVar (user ^. charge)
        say $ concat
            [ user ^. name
            , " has "
            , show ch
            ]

    curPrice <- atomically $ readTVar (auction ^. price)
    Just curTopUser <- atomically $ readTVar (auction ^. topUser)
    say $ concat
        [ "Top User "
        , curTopUser ^. name
        , " wan at price "
        , show curPrice
        , "."
        ]

    result <- checkTotal users auction expected
    if result
        then say "Consistency OK"
        else say "Consistency NG"

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    st <- getPOSIXTime
    let
        et = st + 300
        firstAuctionPrice = 100
        auctionCfg = AuctionConfig st et firstAuctionPrice
        names = ["Alice", "Bob", "Charlie", "David", "Edward"]
        firstCharge = 8000

    auction <- atomically $ newAuction auctionCfg
    users <- forM names $ \nm -> do
        user <- atomically $ newUser nm firstCharge
        th <- async $ userThread user auction
        return (user, th)

    threadDelay $ 500 * 1000

    startAuction auction
    say " ----- Start -----"
    threadDelay $ 1 * 1000 * 1000
    finishAuction auction
    say " ----- Finish -----"

    forM_ users $ \ (_user, thread) -> cancel thread

    say " ----- Summary -----"
    summary (map fst users) auction (firstCharge * length names)
