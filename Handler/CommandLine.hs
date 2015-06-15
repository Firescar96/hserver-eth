{-# LANGUAGE OverloadedStrings #-}

module Handler.CommandLine where

import qualified Data.Text as T

import Import
import qualified Data.List (sort)
import Handler.BlkLast
import System.Process
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)


postCommandLineR::String->String->[String]->Handler Text
postCommandLineR cmd input params = do
  (_, ret, err)  <- liftIO $ readProcessWithExitCode cmd params input
  return $ T.pack $ ret ++ err

postSolcR::Handler Text
postSolcR = do
  maybeVal <- lookupPostParam "src"
  liftIO $ putStrLn $ T.pack $ show maybeVal
  case maybeVal of
    (Just val) -> postCommandLineR "solc" (T.unpack val) ["--binary", "stdout"]
    Nothing -> invalidArgs ["src"]
