{-# LANGUAGE OverloadedStrings #-}

module Handler.QueuedTransactions where

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

getQueuedTransactionsR :: Handler Value
getQueuedTransactionsR  = do
                           addHeader "Access-Control-Allow-Origin" "*"
                           addr <- runDB $ selectList [ RawTransactionBlockNumber ==. (-1) ]
                                   [ LimitTo (fromIntegral $ fetchLimit :: Int), Desc RawTransactionNonce  ] :: Handler [Entity RawTransaction]
                           returnJson $ P.map rtToRtPrime' (P.map entityVal (addr :: [Entity RawTransaction]))

