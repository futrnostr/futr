{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

module Types where

data AppScreen
    = Welcome
    | Account
    | Relay
    | Home
    deriving (Eq, Read, Show)
