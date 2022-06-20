{-# LANGUAGE OverloadedStrings #-}

module Widgets.ProfileImage (
  ImageSize(..), profileImage
) where

import Crypto.Schnorr (XOnlyPubKey, exportXOnlyPubKey)
import Data.Text (Text)
import Monomer

import qualified Data.Text as T

data ImageSize
  = Big
  | Small

profileImage :: WidgetEvent e => Maybe Text -> XOnlyPubKey -> ImageSize -> WidgetNode s e
profileImage picture xo size = image_ path [ fitEither ]
  where
  path = case picture of
    Just p ->
      if p == "" then fallback else p
    Nothing -> fallback
  fallback = do
    let baseUrl = T.pack $ "https://robohash.org/<xo>.png" ++ imageSize size
    let imgUrl x = T.replace "<xo>" (T.pack $ exportXOnlyPubKey x) baseUrl
    imgUrl xo

imageSize :: ImageSize -> String
imageSize Big = "?size=300x300"
imageSize Small = "?size=40x40"
