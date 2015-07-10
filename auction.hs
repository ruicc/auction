#!/usr/bin/env stack
-- stack --resolver=lts-2.16 runghc --package=stm --package=lens --package=random
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses
           , FunctionalDependencies
           , TypeSynonymInstances
           , FlexibleInstances #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Lens
import Control.Lens.TH
import Control.Concurrent
import Control.Concurrent.STM
import Data.Time
import Data.Time.Clock.POSIX
import System.Random
import System.IO


{-
 - オークション
 -
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
    , _auctionCurrentPrice :: TVar Int
    , _auctionCurrentTopUser :: TVar (Maybe User)
    }
data AuctionConfig = AuctionConfig
    { _auctionConfigStartTime :: POSIXTime
    , _auctionConfigEndTime :: POSIXTime
    , _auctionConfigFirstPrice :: Int
    }

makeFields ''User
makeFields ''Auction
makeFields ''AuctionConfig

newUser :: UserName -> Int -> STM User
newUser name charge = do
    ch <- newTVar charge
    return $ User name ch

newAuction :: AuctionConfig -> STM Auction
newAuction cfg = do
    cprice <- newTVar $ cfg ^. firstPrice
    emp <- newTVar Nothing
    return $ Auction
                (cfg ^. startTime)
                (cfg ^. endTime)
                cprice
                emp

bid :: Auction -> User -> Price -> STM Bool
bid auction user bidPrice = do
    curPrice <- readTVar $ auction ^. currentPrice
    if curPrice < bidPrice
        then do
            writeTVar (auction ^. currentPrice) bidPrice
            writeTVar (auction ^. currentTopUser) (Just user)
            return True
        else return False

userThread :: UserName -> Auction -> IO ()
userThread username auction = do
    user <- atomically $ newUser username 100
    forever $ do
        pr <- randomRIO (100, 5000)
        success <- atomically $ bid auction user pr
        case success of
            True -> do
                say $ concat
                    [ user ^. name
                    , " bid at "
                    , show pr
                    , " and succeeded."
                    ]
                threadDelay 500000
            False -> do
--                say $ concat
--                    [ user ^. name
--                    , " bid at "
--                    , show pr
--                    , " and Failed."
--                    ]
                threadDelay 500000

say = putStrLn

main = do
    hSetBuffering stdout LineBuffering
    st <- getPOSIXTime
    let et = st + 300
        auctionCfg = AuctionConfig st et 100
    auction <- atomically $ newAuction auctionCfg

    tids <- forM ["Alice", "Bob", "Charlie", "David", "Edward"] $ \nm -> do
        forkIO $ userThread nm auction

    threadDelay $ 5 * 1000 * 1000

    forM_ tids killThread
