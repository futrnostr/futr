{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

module Types where

import Control.Concurrent (MVar)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Nostr.Keys (KeyPair)

data AppScreen
    = Welcome
    | Account
    | Relay
    | Home
    deriving (Eq, Read, Show)

data AppModel = AppModel
    { keyPair :: Maybe KeyPair
    , seedphrase :: Text
    , currentScreen :: AppScreen
    , errorMsg :: Text
    } deriving (Typeable)

type ModelVar = MVar AppModel
