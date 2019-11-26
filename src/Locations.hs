{-# LANGUAGE OverloadedStrings #-}
module Locations where

import Data.Aeson
import Utils

data Location = LatLong Float Float deriving (Show)
instance ToJSON Location where
  toJSON (LatLong lat long) = object 
    [ "type" .= asStr "geojson"
    , "value" .= object [ "type" .= asStr "Point"
                        , "coordinates" .= [lat, long]
                        ]
    ]

locationEnk :: Location
locationEnk = LatLong 48.1754547 16.4119473
locationLor :: Location
locationLor = LatLong 48.1706739 16.4126072

location :: String -> Location
location l = case take 3 l of
               "Enk" -> locationEnk
               "Lor" -> locationLor
               _ -> LatLong 0 0
