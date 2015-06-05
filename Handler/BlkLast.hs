module Handler.BlkLast where

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


import Handler.Common 
       
import qualified Prelude as P
import Handler.JsonJuggler
         

getBlkLastR :: Integer -> Handler Value
getBlkLastR n      =            do addHeader "Access-Control-Allow-Origin" "*"
                                   blks <- runDB $ E.select $
                                        E.from $ \(a, t) -> do
                                        E.where_ (  a E.^. BlockDataRefBlockId E.==. t E.^. BlockId)
                                        E.limit $ P.min (fromIntegral n :: Int64) fetchLimit 
                                        E.orderBy [E.desc (a E.^. BlockDataRefNumber)]
                                        return t
                                   returnJson $ nub $ P.map bToBPrime' (P.map entityVal (blks :: [Entity Block])) -- consider removing nub - it takes time n^{2}
                                
