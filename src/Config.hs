{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Config
where

import Data.Default
import System.Envy
import GHC.Generics
import System.Environment.Blank
import qualified Data.ByteString.Char8 as B

data App = App
  { csvBaseUrl :: String
  , csvUrlPrefix :: String
  , csvIndexUrl :: String
  , certificateCrt :: FilePath
  , certificateKey :: FilePath
  , certificateStore :: FilePath
  , managerHostName :: String
  , orionUrl :: String
  , fiwareService :: B.ByteString
  } deriving (Generic, Show)

instance Default App where
  def = App
    { csvBaseUrl =  csvBaseUrl
    , csvUrlPrefix = csvUrlPrefix
    , csvIndexUrl = csvBaseUrl ++ csvUrlPrefix
    , certificateCrt = "cert.crt"
    , certificateKey = "cert.key"
    , certificateStore = "stp.wien.gv.at.crt"
    , managerHostName = "stp.wien.gv.at"
    , orionUrl = "http://entrance.docker:8080/contextbroker/v2/entities"
    , fiwareService = "vienna_buildings"
    } where
      csvBaseUrl =  "https://stp.wien.gv.at:4543"
      csvUrlPrefix = "/emscsv/EXPORT/"

instance FromEnv App

instance DefConfig App where
  defConfig = def
