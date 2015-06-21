{-# LANGUAGE OverloadedStrings #-}

module Handler.Solc where

import qualified Data.Text as T

import Control.Monad.Trans.Either

import Data.Char

import Import
import qualified Data.List (sort)
import Handler.BlkLast
import System.Process
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

runSolc::String->String->EitherT String IO [(String, String)]
runSolc theType input = do
  (_, ret, err)  <- liftIO $ readProcessWithExitCode "solc" ["--" ++ theType, "stdout"] input
  if not (null err)
    then left err
    else return $ shatter $ lines ret

getResponse::String->EitherT String IO String
getResponse val = do
  abis <- runSolc "json-abi" val
  compiled <- runSolc "binary" val
  return $ 
    "{\"abi\": " ++ abisToJSON abis ++ ", \"contracts\": " ++ contractsToJSON compiled ++ "}"


postSolcR::Handler Text
postSolcR = do
  maybeVal <- lookupPostParam "src"
  liftIO $ putStrLn $ T.pack $ show maybeVal
  case maybeVal of
    (Just val) -> do
      result <- liftIO $ runEitherT $ getResponse $ T.unpack val
      case result of
        Left err -> return $ T.pack $ "{\"error\": " ++ show err ++ "}"
        Right val -> return $ T.pack val
    Nothing -> invalidArgs ["Missing 'src'"]


--This should be written using a JSON lib, but that feels too heavyweight right now....
--This decision may lead to trouble with chars that need to be escaped, but
--for now I don't think such chars are possible.
contractToJSON::(String, String)->String
contractToJSON (name, hexBytes) =
  "{\"name\": \"" ++ name ++ "\", \"bin\": \"" ++ filter (not . isSpace) hexBytes ++ "\"}"

contractsToJSON::[(String, String)]->String
contractsToJSON contracts = "[" ++ intercalate ", " (map contractToJSON contracts) ++ "]"

abiToJSON::(String, String)->String
abiToJSON (name, abiJSON) =
  "{\"name\": \"" ++ name ++ "\", \"abi\": " ++ abiJSON ++ "}"

abisToJSON::[(String, String)]->String
abisToJSON abis = "[" ++ intercalate ", " (map abiToJSON abis) ++ "]"

---------

isHeader::String->Bool
isHeader ('=':'=':'=':'=':'=':'=':'=':' ':_) = True
isHeader _ = False

getName::String->String
getName = filter (not . (== '=')) . filter (not . (== ' '))

shatter::[String]->[(String, String)]
shatter [] = []
shatter ("":rest) = shatter rest
shatter (header:theType:rest) | isHeader header =
  (getName header, unlines content):shatter rest'
  where
    (content, rest') = break isHeader rest
shatter x = error $ "Missing case in shatter: " ++ show x
