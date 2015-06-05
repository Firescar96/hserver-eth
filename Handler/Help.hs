module Handler.Help where

import Import
import Yesod.Core.Handler

-- getHelpR :: Handler Html
-- getHelpR = defaultLayout $ do
--     sendFile "text/html" "static/help.html"


getHelpR :: Handler Html
getHelpR = do


    defaultLayout $ do
        aDomId <- newIdent
        setTitle "BlockApps.net"
        $(widgetFile "help")