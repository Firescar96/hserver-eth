{-# LANGUAGE OverloadedStrings #-}


module Handler.BlkNumber where

import Import

import Handler.Common 
import Blockchain.Data.DataDefs

import Data.Aeson

import Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import qualified Database.Esqueleto as E

import Data.List
       
import qualified Prelude as P

import Handler.JsonJuggler
       
getBlkNumberR :: Integer -> Handler Value
getBlkNumberR n       =         do addHeader "Access-Control-Allow-Origin" "*"
                                   blks <- runDB $ E.selectDistinct $
                                        E.from $ \(a, t) -> do
                                        E.where_ ( (a E.^. BlockDataRefNumber E.==. E.val n ) E.&&. ( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId))
                                        return t
                                   returnJson $ nub $ P.map bToBPrime' (P.map entityVal (blks :: [Entity Block] )) -- consider removing nub - it takes time n^{2}