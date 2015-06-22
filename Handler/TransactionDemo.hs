module Handler.TransactionDemo where

import Text.Julius

import qualified Data.Text as T

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

postTransactionDemoR :: Handler Html
postTransactionDemoR = do
  maybeABI <- lookupPostParam "abi"
  maybeContractAddress <- lookupPostParam "contractAddress"

  case (maybeABI, maybeContractAddress) of
    (Nothing, _) -> invalidArgs ["Missing 'abi'"]
    (_, Nothing) -> invalidArgs ["Missing 'contractAddress'"]
    (Just abi, Just contractAddress) -> defaultLayout $ do
      liftIO $ putStrLn $ "contractAddress: " ++ contractAddress
  
      setTitle "Transaction Demo"
      $(widgetFile "transactionDemo")

