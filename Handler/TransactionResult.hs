{-# LANGUAGE OverloadedStrings #-}


module Handler.TransactionResult where

import Import

import Data.Aeson
import Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import Handler.Common
import Blockchain.Data.DataDefs
import Blockchain.SHA
import Blockchain.Data.TransactionResult
import Data.List
       
import Handler.JsonJuggler

import qualified Prelude as P
       
getTransactionResultR :: SHA -> Handler Value
getTransactionResultR txHash      = do
  addHeader "Access-Control-Allow-Origin" "*"
  acc <- runDB $ selectList [ TransactionResultTransactionHash ==. txHash ] [] :: Handler [Entity TransactionResult]   
  returnJson $ P.map entityVal acc



