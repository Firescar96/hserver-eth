module Handler.BlkHash where

import Import

import Handler.Common 
import Blockchain.Data.DataDefs

import Data.Aeson
import Blockchain.Data.RLP
import Blockchain.Database.MerklePatricia
import Blockchain.ExtWord
import Blockchain.Util
import Blockchain.SHA

import Data.ByteString.Base16 as B16
import Data.ByteString.Lazy as BS
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql

import qualified Database.Esqueleto as E

import Data.List
import qualified Data.Text.Encoding as T        
import qualified Prelude as P
import Handler.JsonJuggler

getBlkHashR :: Text -> Handler Value
getBlkHashR h =                 do addHeader "Access-Control-Allow-Origin" "*"
                                   blks <- runDB $ E.selectDistinct $
                                        E.from $ \(a, t) -> do
                                        E.where_ ( (a E.^. BlockDataRefHash E.==. E.val ( SHA . fromIntegral . byteString2Integer . fst. B16.decode $ T.encodeUtf8 $ h ) ) E.&&. ( a E.^. BlockDataRefBlockId E.==. t E.^. BlockId))
                                        return t
                                   returnJson $ nub $ P.map bToBPrime' (P.map entityVal (blks :: [Entity Block])) -- consider removing nub - it takes time n^{2}

