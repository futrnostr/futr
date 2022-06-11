{-# LANGUAGE OverloadedStrings   #-}

module Nostr.Keys where

import           Crypto.Schnorr         (KeyPair, SchnorrSig, XOnlyPubKey, decodeHex, keyPairFromSecKey, secKey, xOnlyPubKey)
import qualified Crypto.Schnorr         as Schnorr
import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString.Lazy   (toStrict)
import           Data.List              (length)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           GHC.Exts               (fromList)

type ProfileName = Text

type CurrentlyActive = Bool

data Keys = Keys KeyPair XOnlyPubKey CurrentlyActive (Maybe ProfileName)
  deriving (Eq, Show)

data UnknownXOnlyPubKey
  = ValidXOnlyPubKey XOnlyPubKey
  | InvalidXOnlyPubKey
  deriving (Eq, Show)

instance Ord Keys where
  compare (Keys a _ _ _) (Keys b _ _ _) =
    compare (Schnorr.getKeyPair a) (Schnorr.getKeyPair b)

instance FromJSON Keys where
  parseJSON = withArray "Keys" $ \arr -> do
    kp <- parseJSON $ arr V.! 0
    xo <- parseJSON $ arr V.! 1
    a  <- parseJSON $ arr V.! 2
    n  <- parseJSON $ arr V.! 3
    return $ Keys kp xo a n

instance ToJSON Keys where
  toJSON (Keys kp xo a n) =
    Array $ fromList
      [ toJSON kp
      , toJSON xo
      , toJSON a
      , toJSON n
      ]

instance FromJSON UnknownXOnlyPubKey where
  parseJSON = withText "unknown XOnlyPubKey" $ \t -> do
    case textToByteStringType t Schnorr.xOnlyPubKey of
      Just xo ->
        return $ ValidXOnlyPubKey xo
      Nothing ->
        return InvalidXOnlyPubKey

instance ToJSON UnknownXOnlyPubKey where
  toJSON (ValidXOnlyPubKey xo) = toJSON xo
  toJSON _ = String ""

instance Ord XOnlyPubKey where
  compare a b =
    compare (Schnorr.getXOnlyPubKey a) (Schnorr.getXOnlyPubKey b)

instance FromJSON XOnlyPubKey where
  parseJSON = withText "XOnlyPubKey" $ \p -> do
    case (textToByteStringType p Schnorr.xOnlyPubKey) of
      Just e -> return e
      _    -> fail "invalid XOnlyPubKey"

instance ToJSON XOnlyPubKey where
  toJSON x = String $ T.pack $ Schnorr.exportXOnlyPubKey x

instance FromJSON KeyPair where
  parseJSON = withText "KeyPair" $ \k -> do
    case (textToByteStringType k Schnorr.secKey) of
      Just k' -> return $ Schnorr.keyPairFromSecKey k'
      _       -> fail "invalid key pair"

instance ToJSON KeyPair where
  toJSON kp = String $ T.pack $ Schnorr.exportSecKey $ Schnorr.deriveSecKey kp

instance FromJSON SchnorrSig where
  parseJSON = withText "SchnorrSig" $ \s -> do
    case (textToByteStringType s Schnorr.schnorrSig) of
      Just s' -> return s'
      _       -> fail "invalid schnorr sig"

instance ToJSON SchnorrSig where
  toJSON s = String $ T.pack $ Schnorr.exportSchnorrSig s

textToByteStringType :: Text -> (ByteString -> Maybe a) -> Maybe a
textToByteStringType t f = case Schnorr.decodeHex t of
  Just bs -> f bs
  Nothing -> Nothing

initialKeys :: Keys
initialKeys = Keys kp xo True Nothing where
  kp = keyPairFromSecKey $ load "fef52b22d4568d9235ebf8a4f35dac54a4e748781441506e133532099dae0ded" secKey
  xo = load "134bdeaf23fe7078d94b2836dcb748e762073d4bc274a2c188a44a3fc29df31c" xOnlyPubKey
  load :: String -> (ByteString -> Maybe a) -> a
  load s f =
    case decodeHex s of
      Just bs ->
        case f bs of
          Just b -> b
          _      -> error "failed to load initial keys"
      Nothing -> error "failed to load initial keys"

sameKeys :: Keys -> Keys -> Bool
sameKeys (Keys _ xo _ _) (Keys _ xo' _ _) = xo == xo'

verifyActiveKeys :: [Keys] -> [Keys]
verifyActiveKeys [] = []
verifyActiveKeys ks =
  case length filteredActive of
    0 -> head filteredInactive : (tail $ filteredInactive)
    1 -> head filteredActive : filteredInactive
    _ -> head filteredActive : (disabledActive ++ filteredInactive)
  where
    filteredActive = filter (\(Keys _ _ active _) -> active == True) ks
    filteredInactive = filter (\(Keys _ _ active _) -> active /= True) ks
    disabledActive = disableKeys $ tail filteredActive

disableKeys :: [Keys] -> [Keys]
disableKeys ks = map (\(Keys kp xo _ n) -> Keys kp xo False n) ks
