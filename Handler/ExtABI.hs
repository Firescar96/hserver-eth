{-# LANGUAGE OverloadedStrings #-}

module Handler.ExtABI where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Monad.Trans.Either
import Data.Char
import Import
import qualified Data.List (sort)
import Blockchain.Solidity.ABI
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson.Encode.Pretty as J

postExtABIR::Handler Text
postExtABIR = do
  addHeader "Access-Control-Allow-Origin" "*"

  maybeVal <- lookupPostParam "src"
  liftIO $ putStrLn $ T.pack $ show maybeVal
  case maybeVal of
    (Just val) -> do
      let result = map makeContractSymbolTable <$> getABI ("") (T.unpack $ val)
      case result of
        Left err -> return $ T.pack $ "{\"error\": " ++ show err ++ "}"
        Right v -> return $ T.decodeUtf8 $ BL.toStrict $ J.encodePretty v
    Nothing -> invalidArgs ["Missing 'src'"]

