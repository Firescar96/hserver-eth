{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}

module Handler.AfterSubmission where

import qualified Data.Text as T

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

postAfterSubmissionR::Handler Html
postAfterSubmissionR = do
  maybeABI <- lookupPostParam "abi"
  maybeContractAddress <- lookupPostParam "contractAddress"
  liftIO $ putStrLn $ T.pack $ show maybeABI
  case (maybeABI, maybeContractAddress) of
    (Nothing, _) -> invalidArgs ["Missing 'abi'"]
    (_, Nothing) -> invalidArgs ["Missing 'contractAddress'"]
    (Just abi, Just contractAddress) -> defaultLayout $ do
      setTitle "File Processor"
      addScriptRemote "/static/js/afterSubmission.js"
      $(widgetFile "afterSubmission")


