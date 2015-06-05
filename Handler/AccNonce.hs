module Handler.AccNonce where

import Import

import Database.Persist       
import Database.Persist.TH
import Database.Persist.Postgresql
import Blockchain.Data.DataDefs
import qualified Prelude as P
import qualified Database.Esqueleto as E

import Data.List
import Handler.Common

import qualified Data.Text as T
import Handler.JsonJuggler
       
-- Parses addresses from hex      
getAccNonceR :: Integer -> Handler Value
getAccNonceR nonce =   do
                           addHeader "Access-Control-Allow-Origin" "*"
                           blks <- runDB $ E.selectDistinct $
                                E.from $ \(acc) -> do
                                E.where_ ( (acc E.^. AddressStateRefNonce E.==. E.val nonce ) )
                                E.limit $ fetchLimit
                                return acc
                           returnJson $ nub $ P.map asrToAsrPrime (P.map entityVal blks :: [AddressStateRef]) -- consider removing nub - it takes time n^{2}


