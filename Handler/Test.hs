module Handler.Test where

import Import
import Yesod.Core.Handler

getTestR :: Handler Html
getTestR = defaultLayout $ do
    sendFile "text/html" "static/testreport.html"