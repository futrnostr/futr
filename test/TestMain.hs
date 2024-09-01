{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty

import qualified Nostr.EncryptionTest as EncryptionTest

main :: IO ()
main = do
    encryptionTests <- EncryptionTest.tests
    defaultMain encryptionTests
