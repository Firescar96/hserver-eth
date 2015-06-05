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

module Handler.AccountInfo where

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

getAccountInfoR :: Handler Value
getAccountInfoR = do
                 getParameters <- reqGetParams <$> getRequest

                 let offset = (fromIntegral $ (maybe 0 id $ extractPage "page" getParameters)  :: Int64)
                 let index = (fromIntegral $ (maybe 0 id $ extractPage "index" getParameters)  :: Integer)

                 -- liftIO $ traceIO $ "parameters: " P.++ show getParameters
                 -- liftIO $ traceIO $ "index: " P.++ show index
                 -- liftIO $ traceIO $ "offset: " P.++ show offset

                 addHeader "Access-Control-Allow-Origin" "*"

                 addrs <- runDB $ E.selectDistinct $
                                        E.from $ \(accStateRef) -> do
                        
                                        E.where_ ((P.foldl1 (E.&&.) $ P.map (getAccFilter (accStateRef)) $ getParameters ))

                                        E.offset $ (limit * offset)
                                        E.limit $ limit

                                        E.orderBy [E.desc (accStateRef E.^. AddressStateRefBalance)]

                                        return accStateRef
                 --liftIO $ traceIO $ "number of results: " P.++ (show $ P.length addrs)
                 returnJson $ nub $ P.map asrToAsrPrime (P.map entityVal (addrs :: [Entity AddressStateRef])) -- consider removing nub - it takes time n^{2}
                 where 
                   limit = (fromIntegral $ fetchLimit :: Int64)
                  