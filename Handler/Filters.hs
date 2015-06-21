module Handler.Filters where

import qualified Database.Esqueleto as E

import qualified Prelude as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Aeson
import Data.Binary as Bin
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Base16 as B16
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql
import Numeric
import Blockchain.Data.RLP
import Blockchain.Database.MerklePatricia
import Blockchain.ExtWord
import Blockchain.Util
import Blockchain.SHA


import Blockchain.Data.RLP
import Blockchain.Database.MerklePatricia
import Blockchain.ExtWord
import Blockchain.Util
import Blockchain.Data.DataDefs
import Blockchain.Data.Address

import Control.Monad
import Data.Set


import Import

getBlkFilter :: (E.Esqueleto query expr backend) => (expr (Entity BlockDataRef), expr (Entity AddressStateRef), expr (Entity RawTransaction), expr (Entity Block))-> (Text, Text) -> expr (E.Value Bool)
getBlkFilter  _                               ("page", v)    = E.val True 
getBlkFilter  _                               ("index", v)    = E.val True 
getBlkFilter  _                               ("raw", v)    = E.val True 
getBlkFilter  _                               ("next", v)    = E.val True
getBlkFilter  _                               ("prev", v)    = E.val True
getBlkFilter  _                               ("appname", v) = E.val True

getBlkFilter (bdRef, _, _, _)                 ("ntx", v)    = bdRef E.^. BlockDataRefNumber E.==. E.val (P.read $ T.unpack v :: Integer)

getBlkFilter (bdRef, _, _, _)                 ("number", v)    = bdRef E.^. BlockDataRefNumber E.==. E.val (P.read $ T.unpack v :: Integer)
getBlkFilter (bdRef, _, _, _)                 ("minnumber", v)    = bdRef E.^. BlockDataRefNumber E.>=. E.val (P.read $ T.unpack v :: Integer)
getBlkFilter (bdRef, _, _, _)                 ("maxnumber", v)    = bdRef E.^. BlockDataRefNumber E.<=. E.val (P.read $ T.unpack v :: Integer)

getBlkFilter (bdRef, _, _, _)                 ("gaslim", v)    = bdRef E.^. BlockDataRefGasLimit E.==. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("mingaslim", v) = bdRef E.^. BlockDataRefGasLimit E.>=. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("maxgaslim", v) = bdRef E.^. BlockDataRefGasLimit E.<=. E.val (P.read $ T.unpack v :: Integer) 

getBlkFilter (bdRef, _, _, _)                 ("gasused", v)    = bdRef E.^. BlockDataRefGasUsed E.==. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("mingasused", v) = bdRef E.^. BlockDataRefGasUsed E.>=. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("maxgasused", v) = bdRef E.^. BlockDataRefGasUsed E.<=. E.val (P.read $ T.unpack v :: Integer) 

getBlkFilter (bdRef, _, _, _)                 ("diff", v)      = bdRef E.^. BlockDataRefDifficulty E.==. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("mindiff", v)   = bdRef E.^. BlockDataRefDifficulty E.>=. E.val (P.read $ T.unpack v :: Integer) 
getBlkFilter (bdRef, _, _, _)                 ("maxdiff", v)   = bdRef E.^. BlockDataRefDifficulty E.<=. E.val (P.read $ T.unpack v :: Integer) 

-- getBlkFilter (bdRef, accStateRef, rawTX, blk) ("time", v)      = bdRef E.^. BlockDataRefTimestamp E.==. E.val (stringToDate v)
-- getBlkFilter (bdRef, accStateRef, rawTX, blk) ("mintime", v)   = bdRef E.^. BlockDataRefTimestamp E.>=. E.val (stringToDate v)
-- getBlkFilter (bdRef, accStateRef, rawTX, blk) ("maxtime", v)   = bdRef E.^. BlockDataRefTimestamp E.<=. E.val (stringToDate v)

getBlkFilter (bdRef, accStateRef, rawTX, blk) ("txaddress", v) = (rawTX E.^. RawTransactionBlockId E.==. blk E.^. BlockId)
                                                              E.&&. ((rawTX E.^. RawTransactionFromAddress E.==. E.val (toAddr v)))
                                                                      E.||. (rawTX E.^. RawTransactionToAddress E.==. E.val (Just (toAddr v)))                                                      
getBlkFilter (bdRef, accStateRef, rawTX, blk) ("coinbase", v)  = bdRef E.^. BlockDataRefCoinbase E.==. E.val (toAddr v)
                                                              E.&&. ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId)
getBlkFilter (bdRef, accStateRef, rawTX, blk) ("address", v)   = accStateRef E.^. AddressStateRefAddress E.==. E.val (toAddr v)
getBlkFilter (bdRef, accStateRef, rawTX, blk) ("blockid", v)   = bdRef E.^. BlockDataRefBlockId E.==. E.val (toBlockId v)
                                                              E.&&. ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId)
getBlkFilter (bdRef, accStateRef, rawTX, blk) ("hash", v)   = (bdRef E.^. BlockDataRefHash E.==. E.val ( SHA . fromIntegral . byteString2Integer . fst. B16.decode $ T.encodeUtf8 $ v ) ) E.&&. ( bdRef E.^. BlockDataRefBlockId E.==. blk E.^. BlockId)
                                        

getAccFilter :: (E.Esqueleto query expr backend) => (expr (Entity AddressStateRef))-> (Text, Text) -> expr (E.Value Bool)
getAccFilter (accStateRef) ("page", v)         =  E.val True
getAccFilter (accStateRef) ("index", v)        =  E.val True
getAccFilter (accStateRef) ("raw", v)          =  E.val True
getAccFilter (accStateRef) ("next", v)         =  E.val True
getAccFilter (accStateRef) ("prev", v)         =  E.val True
getAccFilter  _            ("appname", v)      = E.val True
--getAccFilter (accStateRef) ("RawTransactionCodeOrData", v)    =  E.val True

getAccFilter (accStateRef) ("balance", v)      = accStateRef E.^. AddressStateRefBalance E.==. E.val (P.read $ T.unpack v :: Integer) 
getAccFilter (accStateRef) ("minbalance", v)   = accStateRef E.^. AddressStateRefBalance E.>=. E.val (P.read $ T.unpack v :: Integer) 
getAccFilter (accStateRef) ("maxbalance", v)   = accStateRef E.^. AddressStateRefBalance E.<=. E.val (P.read $ T.unpack v :: Integer) 

getAccFilter (accStateRef) ("nonce", v)        = accStateRef E.^. AddressStateRefNonce E.==. E.val (P.read $ T.unpack v :: Integer)
getAccFilter (accStateRef) ("minnonce", v)     = accStateRef E.^. AddressStateRefNonce E.>=. E.val (P.read $ T.unpack v :: Integer)
getAccFilter (accStateRef) ("maxnonce", v)     = accStateRef E.^. AddressStateRefNonce E.<=. E.val (P.read $ T.unpack v :: Integer)

getAccFilter (accStateRef) ("address", v)      = accStateRef E.^. AddressStateRefAddress E.==. E.val (toAddr v)

getTransFilter :: (E.Esqueleto query expr backend) => (expr (Entity RawTransaction))-> (Text, Text) -> expr (E.Value Bool)
getTransFilter (rawTx)     ("page", v)         = E.val True
getTransFilter (rawTx)     ("index", v)        = E.val True
getTransFilter (rawTx)     ("raw", v)          = E.val True
getTransFilter (rawTx)     ("next", v)         = E.val True
getTransFilter (rawTx)     ("prev", v)         = E.val True
getTransFilter  _          ("appname", v)      = E.val True

getTransFilter (rawTx)     ("address", v)      = rawTx E.^. RawTransactionFromAddress E.==. E.val (toAddr v) E.||. rawTx E.^. RawTransactionToAddress E.==. E.val (Just (toAddr v))
getTransFilter (rawTx)     ("from", v)         = rawTx E.^. RawTransactionFromAddress E.==. E.val (toAddr v)
getTransFilter (rawTx)     ("to", v)           = rawTx E.^. RawTransactionToAddress E.==. E.val (Just (toAddr v))
getTransFilter (rawTx)     ("hash", v)         = rawTx E.^. RawTransactionTxHash  E.==. E.val ( SHA . fromIntegral . byteString2Integer . fst. B16.decode $ T.encodeUtf8 $ v ) 

--getTransFilter (rawTx)     ("type", "Contract") = (rawTx E.^. RawTransactionToAddress E.==. (E.val "")) E.&&. (RawTransactionCodeOrData E.!=. (E.val ""))

getTransFilter (rawTx)     ("gasprice", v)     = rawTx E.^. RawTransactionGasPrice E.==. E.val (P.read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("mingasprice", v)  = rawTx E.^. RawTransactionGasPrice E.>=. E.val (P.read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("maxgasprice", v)  = rawTx E.^. RawTransactionGasPrice E.<=. E.val (P.read $ T.unpack v :: Integer)

getTransFilter (rawTx)     ("gaslimit", v)     = rawTx E.^. RawTransactionGasLimit E.==. E.val (P.read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("mingaslimit", v)  = rawTx E.^. RawTransactionGasLimit E.>=. E.val (P.read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("maxgaslimit", v)  = rawTx E.^. RawTransactionGasLimit E.<=. E.val (P.read $ T.unpack v :: Integer)

getTransFilter (rawTx)     ("value", v)        = rawTx E.^. RawTransactionValue E.==. E.val (P.read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("minvalue", v)     = rawTx E.^. RawTransactionValue E.>=. E.val (P.read $ T.unpack v :: Integer)
getTransFilter (rawTx)     ("maxvalue", v)     = rawTx E.^. RawTransactionValue E.<=. E.val (P.read $ T.unpack v :: Integer)

getTransFilter (rawTx)     ("blockid", v)      = rawTx E.^. RawTransactionBlockId E.==. E.val (toBlockId v)
getTransFilter (rawTx)     ("blocknumber", v)  = rawTx E.^. RawTransactionBlockNumber E.==. E.val (P.read $ T.unpack v :: Int)

getStorageFilter :: (E.Esqueleto query expr backend) => (expr (Entity Storage), expr (Entity AddressStateRef)) -> (Text, Text) -> expr (E.Value Bool)
getStorageFilter _ ("page",_)  = E.val True
getStorageFilter _ ("index",_) = E.val True
getStorageFilter (storage,_) ("key", v)
  = storage E.^. StorageKey E.==. E.val (P.fromIntegral (P.read $ T.unpack v :: Integer) :: Word256)
getStorageFilter (storage,_) ("minkey", v)
  = storage E.^. StorageKey E.>=. E.val (P.fromIntegral (P.read $ T.unpack v :: Integer) :: Word256) 
getStorageFilter (storage,_) ("maxkey", v)
  = storage E.^. StorageKey E.<=. E.val (P.fromIntegral (P.read $ T.unpack v :: Integer) :: Word256)
getStorageFilter (storage,_) ("keystring", v)
  = storage E.^. StorageKey E.==. E.val (Bin.decode $ BS.fromStrict $ T.encodeUtf8 v :: Word256)
getStorageFilter (storage,_) ("keyhex", v)
  = storage E.^. StorageKey E.==. E.val (fromHexText v)
getStorageFilter (storage,_) ("value", v)
  = storage E.^. StorageValue E.==. E.val (P.fromIntegral (P.read $ T.unpack v :: Integer) :: Word256)
getStorageFilter (storage,_) ("minvalue", v)
  = storage E.^. StorageValue E.>=. E.val (P.fromIntegral (P.read $ T.unpack v :: Integer) :: Word256)
getStorageFilter (storage,_) ("maxvalue", v)
  = storage E.^. StorageValue E.<=. E.val (P.fromIntegral (P.read $ T.unpack v :: Integer) :: Word256)
getStorageFilter (storage,_) ("valuestring", v)
  = storage E.^. StorageValue E.==. E.val (Bin.decode $ BS.fromStrict $ T.encodeUtf8 v :: Word256)
getStorageFilter (storage,_) ("addressid", v)
  = storage E.^. StorageAddressStateRefId E.==. E.val (toAddrId v)
getStorageFilter (_,addrStRef) ("address", v)      -- Note: a join is done in StorageInfo
  = addrStRef E.^. AddressStateRefAddress E.==. E.val (toAddr v)  


toAddrId = toBlockId
toBlockId v = toSqlKey (fromIntegral $ (P.read $ T.unpack v :: Integer) )

toAddr v = Address wd160
  where ((wd160, _):_) = readHex $ T.unpack $ v ::  [(Word160,String)]

fromHexText :: T.Text -> Word256
fromHexText v = res 
  where ((res,_):_) = readHex $ T.unpack $ v :: [(Word256,String)]

extractPage :: String -> [(Text, Text)] ->  Maybe Integer 
extractPage name ts = Control.Monad.foldM toFold 0 (P.map selectPage ts)
     where 
       toFold :: Integer -> Maybe Integer -> Maybe Integer
       toFold n Nothing = Just n
       toFold n (Just m) = Just (P.maximum [n, m])
       selectPage :: (Text, Text) -> Maybe Integer
       selectPage (s, v) | T.unpack s == name = Just (P.read $ T.unpack v :: Integer)
                         | otherwise = Nothing
       selectPage (_, v) = Nothing


toParam a = Param a
fromParam (Param a) = a

data Param = Param (Text,Text)
instance Eq Param where
  Param a == Param b = fst a == fst b
instance Ord Param where
  (Param a) `compare` (Param b) = (fst a) `compare` (fst b)

appendIndex :: [(Text, Text)] -> [(Text,Text)] -- this sould be using URL encoding code from Yesod
appendIndex l = P.map fromParam (Data.Set.toList $ Data.Set.insert (toParam ("index", "")) $ Data.Set.fromList $ P.map toParam l)

extraFilter :: (Text,Text) -> Text -> (Text,Text)
extraFilter ("index", v) v' = ("index", v')
extraFilter (a,b) v'        = (a,b)

getBlockNum :: Block -> Integer
getBlockNum (Block (BlockData ph uh cb@(Address a) sr tr rr lb d num gl gu ts ed non mh) rt bu) = num 

getTxNum :: RawTransaction -> Int
getTxNum (RawTransaction (Address fa) non gp gl ta val cod r s v bid bn h) = bn

if' :: Bool -> a -> b -> Either a b
if' x a b = if x == True then Left a else Right b
