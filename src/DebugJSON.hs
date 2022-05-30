{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DebugJSON where

import           Crypto.Schnorr                       (XOnlyPubKey, KeyPair,
                                                       decodeHex, deriveXOnlyPubKey,
                                                       keyPairFromSecKey, generateKeyPair,
                                                       secKey, xOnlyPubKey)
import           Data.Aeson
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as LazyBytes
import           Data.Either                          (fromRight)
import           Data.Maybe
import           Data.Text                            (Text, strip)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (encodeUtf8)

import           Nostr.Event
import           Nostr.Filter
import           Nostr.Keys
import           Nostr.Profile
import           Nostr.Relay
import           Nostr.Request  (Request(..), Subscription(..))


main = do
     let e = "{\"id\":\"a948e858fcdb8c8e5dcc88c387cc37f27872fad63473bb8d2ea77745bac61e39\",\"pubkey\":\"a5b365b10946f00071d56c50f86ab2e36d3e95406f04135814969c59fb0e7800\",\"created_at\":1653201423,\"kind\":3,\"tags\":[[\"p\",\"a5b365b10946f00071d56c50f86ab2e36d3e95406f04135814969c59fb0e7800\",\"\",\"oliverP\"]],\"content\":\"\",\"sig\":\"f99071b5c8e31f202816da7738421cb3ef4719484c659a54974f8bbc73061637db4b9235d3676720a384b32a3e57bd9756e2d35db6a1857b29b55872081dec92\"}";
--     -- let d = parseE e
     let d = parseE e
     putStrLn $ show d
--
parseE :: LazyBytes.ByteString -> Either String Event
parseE = eitherDecode


-- main = do
--     let e = "\"c35809ce922d2883a12cb7fe1826237c268556549b2f3ff1166e1fcbba1f7592\"";
--     -- let d = parseE e
--     let d = parseE e
--     putStrLn $ show d
--
-- parseE :: LazyBytes.ByteString -> Either String XOnlyPubKey
-- parseE = eitherDecode



{- for debugging json parsers
main = do
    let e = "[\"EVENT\",\"044039d07ff47f100e29debaec66a3cd35e02b0cb849a3bce2cfd8bc0a1629f1\",{\"id\":\"763500a66f5ed2cc3bf77879082dc406a119ad177d12c2727e671a397a60fcfb\"
,\"pubkey\":\"1702e3b17de25d9fd63d80fb1a2394a26239cb2a747b893e82f776704d888c4b\",\"created_at\":1649731192,\"kind\":1,\"tags\":[[\"e\",\"d87c30cd198635dad4e6981b907fa4ea2
608a6e675844ec798f85ca6bafa2a34\",\"\"]],\"content\":\"ff\",\"sig\":\"54436b7cd3afbb5c9bb7e3da455725a27c8f064acc52febe9d031918d07135acbd001192751e19579ec7cdbe76ff36631e26
f02f25da94a5c5a9cc6f2ffe97c8\"}]\n";
    let d = parseE e
    putStrLn $ show d

parseE :: LazyBytes.ByteString -> Either String ServerResponse
parseE = eitherDecode
-}
