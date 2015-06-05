{-# LANGUAGE OverloadedStrings #-}

module Handler.TxLast where

import Import


import Handler.Common
import Blockchain.Data.DataDefs

import Database.Persist       
import Database.Persist.TH
import Database.Persist.Postgresql
import qualified Prelude as P
import Blockchain.Data.Address
import Blockchain.Data.RawTransaction
import Blockchain.ExtWord
import Numeric

import qualified Data.Text as T

import qualified Database.Esqueleto as E
import Handler.JsonJuggler

-- Parses addresses from hex      
getTxLastR = (getTxLastR' 1)

getTxLastR' ::  Integer -> Handler Value
getTxLastR' num = do
                           addHeader "Access-Control-Allow-Origin" "*"
                           tx <- runDB $ E.select $
                                 E.from $ \rawTX -> do
                                 E.limit $ P.min (fromIntegral num :: Int64) fetchLimit 
                              --   E.offset $ (limit * off)
                                 E.orderBy [E.desc (rawTX E.^. RawTransactionBlockId)]  
                                 return rawTX
                           returnJson $ P.map rtToRtPrime' (P.map entityVal (tx :: [Entity RawTransaction]))
