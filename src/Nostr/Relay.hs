{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Nostr.Relay where

import           Basement.IntegralConv
import           Control.Lens
import           Data.Maybe    (fromJust)
import           Data.Text     (Text, append, pack)
import           Text.URI      (URI, render)
import qualified Text.URI      as URI
import           Text.URI.Lens
import qualified Text.URI.QQ   as QQ

data RelayInfo = RelayInfo
  { readable  :: Bool
  , writable  :: Bool
  }
  deriving (Eq, Show)

data Relay = Relay
  { uri       :: URI
  , info      :: RelayInfo
  , connected :: Bool
  }
  deriving (Show)

instance Eq Relay where
  (Relay r _ _) == (Relay r' _ _) = r == r'

instance Ord Relay where
  compare (Relay r _ _) (Relay r' _ _) = compare r r'

defaultPool :: [Relay]
defaultPool =
  [
  --   Relay
  --   { host = "nostr-pub.wellorder.net"
  --   , port = 443
  --   , secure = True
  --   , readable = True
  --   , writable = True
  --   , connected = False
  --   }
  -- ,
    Relay
    { uri = [QQ.uri|ws://localhost:2700|]
    , info = RelayInfo True True
    , connected = False
    }
  ]

poolWithoutRelay :: [Relay] -> Relay -> [Relay]
poolWithoutRelay p r = filter (\r' -> not $ r == r') p

relayName :: Relay -> Text
relayName r = render $ uri r

isValidRelayURI :: URI -> Bool
isValidRelayURI u =
  case u ^. uriScheme of
    (Just s) -> if (URI.unRText s) `elem` ["ws", "wss"] then True else False
    _            -> False

extractScheme :: Relay -> Text
extractScheme r = URI.unRText scheme
  where
    scheme = fromJust $ uri' ^. uriScheme
    uri' = uri r

extractHostname :: Relay -> Text
extractHostname r =
  URI.unRText $ fromJust $ uri' ^? uriAuthority . _Right . authHost
  where
    uri' = uri r

extractPort :: Relay -> Int
extractPort r =
  case uri' ^? uriAuthority . _Right . authPort of
    Just (Just p) -> wordToInt p
    _ -> if extractScheme r == "wss" then 443 else 80
  where
    uri' = uri r

extractPath :: Relay -> Text
extractPath r =
  case uri' ^? uriPath of
    Just [] -> "/"
    Just p  -> foldl (\x y -> x `append` "/" `append` y ) "" (map URI.unRText p)
    _       -> "/"
  where
    uri' = uri r
