{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent  (forkIO)
import Control.Lens
import Control.Monad       (forever, unless)
import Control.Monad.Trans (liftIO)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import Monomer
import Network.WebSockets (ClientApp, Connection, receiveData, sendClose, sendTextData)
import TextShow
import Wuss
import qualified Monomer.Lens as L


data Relay = Relay
    { host :: String
    , port :: Int
    , readable :: Bool
    , writable :: Bool
    } deriving Show

defaultPool =
    [ Relay { host="relayer.fiatjaf.com", port=443, readable=True, writable=True }
    , Relay { host="nostr-pub.wellorder.net", port=443, readable=True, writable=True }
    ]


newtype AppModel = AppModel {
  _clickCount :: Int
} deriving (Eq, Show)

data AppEvent
  = AppInit
  | AppIncrease
  | Connected
  deriving (Eq, Show)

makeLenses 'AppModel

buildUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
  widgetTree = vstack [
      label "Hello, nostr",
      label "Haskell GUI Development",
      spacer,
      hstack [
        label $ "Click count: " <> showt (model ^. clickCount),
        spacer,
        button "Increase count" AppIncrease
      ]
    ] `styleBasic` [padding 10]

handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  AppInit -> [
        Task $ runSecureClient "relayer.fiatjaf.com" 443 "/" app,
        Model model
    ]
  AppIncrease -> [Model (model & clickCount +~ 1)]


startClient :: IO AppEvent
startClient = runSecureClient "relayer.fiatjaf.com" 443 "/" app


app :: ClientApp AppEvent
app conn = do
    putStrLn "Connected, nice!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- receiveData conn
        liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ sendTextData conn line >> loop

    loop
    sendClose conn ("Bye!" :: Text)
    return Connected


main :: IO ()
main = do
  startApp model handleEvent buildUI config
  where
    config = [
      appWindowTitle "Hello world",
      appTheme darkTheme,
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appInitEvent AppInit,
      appRenderOnMainThread
      ]
    model = AppModel 0
