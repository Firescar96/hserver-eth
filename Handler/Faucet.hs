{-# LANGUAGE OverloadedStrings #-}

module Handler.Faucet where

import qualified Data.Text as T

import Control.Monad.Trans.Either

import Data.Char
import Network.Haskoin.Crypto

import Import
import Handler.Filters
import System.Process
import qualified Data.ByteString as B
import qualified Data.Binary as BN
import Blockchain.Data.DataDefs
import qualified Database.Esqueleto as E
import Blockchain.Data.Address
import Blockchain.Constants
import Blockchain.Data.Transaction
import Blockchain.Data.BlockDB

import qualified Prelude as P

retrievePrvKey :: FilePath -> IO (Maybe PrvKey)
retrievePrvKey path = do
  keyBytes <- readFile path
  let intVal = BN.decode $ keyBytes :: Integer
  return $ makePrvKey intVal

-- lookupNonce :: Address -> Integer
lookupNonce addr = do
  addrSt <- runDB $ E.select $ 
                      E.from $ \(accStateRef) -> do

                      E.where_ (accStateRef E.^. AddressStateRefAddress E.==. (E.val addr))
                      return accStateRef
  return $ addressStateRefNonce $ E.entityVal $ P.head $ addrSt

postFaucetR :: Handler Text
postFaucetR = do
  addHeader "Access-Control-Allow-Origin" "*"
  
  maybeVal <- lookupPostParam "address"
  liftIO $ putStrLn $ T.pack $ show maybeVal
  case maybeVal of
    (Just val) -> do 
      key <- liftIO $ retrievePrvKey "priv"
      liftIO $ putStrLn $ T.pack $ show key
      case key of
        (Just key') -> do
           nonce <- lookupNonce $ prvKey2Address key'
           liftIO $ putStrLn $ T.pack $ "nonce: " ++ (show nonce)
           tx <- liftIO $ withSource devURandom (createMessageTX nonce (100) (100000) (toAddr val) (1000*ether) "" key')
           liftIO $ putStrLn $ T.pack $ "tx for faucet: " ++ (show tx)
           _ <- runDB $ insert $ tx2RawTX' tx
           return $ "/query/account?address="++ val
        Nothing -> 
           return val

    Nothing -> invalidArgs ["Missing 'address'"]

