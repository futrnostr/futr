{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty

import qualified Nostr.EncryptionTest as EncryptionTest
import qualified Nostr.Nip05SearchTest as Nip05SearchTest

main :: IO ()
main = do
    encryptionTests <- EncryptionTest.tests
    nip05Tests <- Nip05SearchTest.tests
    defaultMain $ testGroup "All Tests"
        [ encryptionTests
        , nip05Tests
        ]
