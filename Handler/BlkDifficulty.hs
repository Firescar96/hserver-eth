module Handler.BlkDifficulty where

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

getBlkDifficultyR :: Integer -> Handler Value
getBlkDifficultyR d      =      do addHeader "Access-Control-Allow-Origin" "*"
                                   blks <- runDB $ E.select $
                                        E.from $ \(a, t) -> do
                                        E.where_ ( (a E.^. BlockDataRefDifficulty E.==. E.val d ) E.&&. ( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId))
                                        E.orderBy [E.desc (a E.^. BlockDataRefNumber)]
                                        E.limit $ fetchLimit
                                        return t
                                   returnJson $ nub $ P.map bToBPrime' (P.map entityVal (blks :: [Entity Block])) -- consider removing nub - it takes time n^{2}
