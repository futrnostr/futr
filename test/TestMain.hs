{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty

import qualified Nostr.EncryptionTest as EncryptionTest
import qualified Nostr.Nip05SearchTest as Nip05SearchTest
import qualified Nostr.Nip05Search.NamecoinTest as NamecoinTest

main :: IO ()
main = do
    encryptionTests <- EncryptionTest.tests
    nip05Tests <- Nip05SearchTest.tests
    namecoinTests <- NamecoinTest.tests
    defaultMain $ testGroup "All Tests"
        [ encryptionTests
        , nip05Tests
        , namecoinTests
        ]
