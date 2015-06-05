{-# LANGUAGE DeriveDataTypeable
           , EmptyDataDecls
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , MultiParamTypeClasses
           , TypeFamilies
           , UndecidableInstances
           , GADTs
 #-}

module Handler.BlkTxAddress where

import Import

import Handler.Common 
import Blockchain.Data.DataDefs
import Blockchain.Data.Address

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql
import Numeric
import Blockchain.Data.RLP
import Blockchain.Database.MerklePatricia
import Blockchain.ExtWord
import Blockchain.Util

import Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BS

import qualified Database.Esqueleto as E
       
import Data.List

import Control.Monad

import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import System.Locale
import Data.Time
import Data.Time.Format


import Blockchain.Data.Address
import Blockchain.ExtWord
import Numeric

import Yesod.Core.Handler

-- import Debug.Trace
import Handler.JsonJuggler

getBlkTxAddressR a = (getBlkTxAddressR' a 0)

getBlkTxAddressR' :: Text -> Integer -> Handler Value
getBlkTxAddressR' address offset = do
                   addHeader "Access-Control-Allow-Origin" "*"
                   blks <- runDB $ E.select $
                                        E.from $ \(blk `E.InnerJoin` bdRef `E.FullOuterJoin` rawTX) -> do

                                        E.on ( bdRef E.^. BlockDataRefBlockId E.==. rawTX E.^. RawTransactionBlockId ) 
                                        E.on ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId )                                        
                                        E.where_ (( bdRef E.^. BlockDataRefCoinbase E.==. E.val (Address wd160)) E.||.
                                                  ( ( rawTX E.^. RawTransactionFromAddress E.==. E.val (Address wd160))
                                                      E.||. ( rawTX  E.^. RawTransactionToAddress E.==. E.val (Just (Address wd160) ))))

                                        E.limit $ (fetchLimit)
                                        E.offset $ (limit * off)

                                        E.orderBy [E.desc (bdRef E.^. BlockDataRefNumber)]

                                        return blk
                   returnJson $ nub $ P.map bToBPrime' (P.map entityVal (blks :: [Entity Block])) 

        where
          ((wd160, _):_) = readHex $ T.unpack $ address ::  [(Word160,String)]
          limit = (fromIntegral $ fetchLimit :: Int64)
          off = (fromIntegral $ offset :: Int64)
                    
