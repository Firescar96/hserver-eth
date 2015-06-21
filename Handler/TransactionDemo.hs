module Handler.TransactionDemo where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

postTransactionDemoR :: Handler Html
postTransactionDemoR = do
  maybeABI <- lookupPostParam "abi"
  case maybeABI of
    Nothing -> invalidArgs ["missing abi"]
    Just abi -> 
      defaultLayout $ do
          aDomId <- newIdent
          setTitle "BlockApps.net"
          $(widgetFile "transactionDemo")

