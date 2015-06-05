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


module Handler.TransactionInfo where

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

import Debug.Trace
import Handler.JsonJuggler

import Handler.Filters

getTransactionInfoR :: Handler Value
getTransactionInfoR = do
                 getParameters <- reqGetParams <$> getRequest

                 let offset = (fromIntegral $ (maybe 0 id $ extractPage "page" getParameters)  :: Int64)
                 let index  = (fromIntegral $ (maybe 0 id $ extractPage "index" getParameters)  :: Int)
                 let raw    = (fromIntegral $ (maybe 0 id $ extractPage "raw" getParameters) :: Integer) > 0

                 -- liftIO $ traceIO $ "parameters: " P.++ show getParameters
                 -- liftIO $ traceIO $ "index: " P.++ show index
                 -- liftIO $ traceIO $ "offset: " P.++ show offset

                 addHeader "Access-Control-Allow-Origin" "*"
                 txs <- runDB $ E.select $
                                        E.from $ \(rawTx) -> do
                        
                                        E.where_ ((P.foldl1 (E.&&.) $ P.map (getTransFilter (rawTx)) $ getParameters ))

                                        let criteria = P.map (getTransFilter rawTx) $ getParameters 
                                        let allCriteria = ((rawTx E.^. RawTransactionBlockNumber) E.>=. E.val index) : criteria

                                        -- FIXME: if more than `limit` transactions per block, we will need to have a tuple as index
                                        E.where_ (P.foldl1 (E.&&.) allCriteria)

                                        E.offset $ (limit * offset)
                                        E.limit $ (limit)
                                        E.orderBy [E.asc (rawTx E.^. RawTransactionBlockNumber), E.asc (rawTx E.^. RawTransactionNonce)]

                                        return rawTx
                 --liftIO $ traceIO $ "number of results: " P.++ (show $ P.length txs)

                 let modTxs = (nub (P.map entityVal (txs :: [Entity RawTransaction])))
                 let newindex = pack $ show $ 1+(getTxNum $ P.last modTxs)
                 let extra p = P.zipWith extraFilter p (P.repeat (newindex))
                 -- this should actually use URL encoding code from Yesod
                 let next p = "/query/transaction?" P.++  (P.foldl1 (\a b -> (unpack a) P.++ "&" P.++ (unpack b)) $ P.map (\(k,v) -> (unpack k) P.++ "=" P.++ (unpack v)) (extra p))
                 let addedParam = appendIndex getParameters

                 toRet raw modTxs (next addedParam) -- consider removing nub - it takes time n^{2}
               where
                   toRet raw bs gp = case if' raw bs (P.map rtToRtPrime (P.zip (P.repeat gp) bs)) of 
                              Left a -> returnJson a
                              Right b -> returnJson b
                   limit = (fromIntegral $ fetchLimit :: Int64)


                   
                   