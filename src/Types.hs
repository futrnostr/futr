{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

module Types where

data AppScreen
    = KeyMgmt
    | Relay
    | Home
    deriving (Eq, Read, Show)
