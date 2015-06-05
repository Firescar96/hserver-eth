{-# LANGUAGE Rank2Types #-}

module Handler.BlkNumberRange where

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

getFilter :: (E.Esqueleto query expr backend) =>(expr (Entity BlockDataRef), expr (Entity Block))-> (Integer, Integer) -> expr (E.Value Bool)
getFilter (bdr, block) (n1, n2) = ( (bdr E.^. BlockDataRefNumber E.>=. E.val n1 ) E.&&. (bdr E.^. BlockDataRefNumber E.<=. E.val n2)  E.&&. ( bdr E.^. BlockDataRefBlockId E.==. block E.^. BlockId))

getBlkNumberRangeR n1 n2 = (getBlkNumberRangeR' n1 n2 0)

getBlkNumberRangeR' :: Integer -> Integer -> Integer -> Handler Value
getBlkNumberRangeR' n1 n2 offset = do addHeader "Access-Control-Allow-Origin" "*"
                                      blks <- runDB $ E.select $
                                        E.from $ \(bdr, block) -> do
                                      --  E.where_ ( (a E.^. BlockDataRefNumber E.>=. E.val n1 ) E.&&. (a E.^. BlockDataRefNumber E.<=. E.val n2)  E.&&. ( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId))
                                        E.where_ $ getFilter (bdr, block) (n1, n2)
                                        E.orderBy [E.desc (bdr E.^. BlockDataRefNumber)]
                                        E.limit $ limit
                                        E.offset (limit * off)
                                        return block
                                      returnJson $ nub $ P.map bToBPrime' (P.map entityVal (blks :: [Entity Block])) -- consider removing nub - it takes time n^{2}
                                      where 
                                        limit = (fromIntegral $ fetchLimit :: Int64)
                                        off   = (fromIntegral $ offset :: Int64)