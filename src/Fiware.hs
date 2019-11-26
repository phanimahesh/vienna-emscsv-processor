{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Fiware where

import           Control.Lens hiding ((.=))
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Network.Wreq as W
import           Network.Wreq.Types
import RIO

import Config
import CSVParser
import Utils
import Locations

wOpts :: RIO App Network.Wreq.Types.Options
wOpts = do
  service <- asks fiwareService
  return $ W.defaults & W.header "X-PVP-ROLES" .~ ["fiware(" <> service <> "=cb:w+ql:w)"]
                      & W.header "fiware-service" .~ [service]

getFromOrion :: String -> RIO App (W.Response BL.ByteString)
getFromOrion entityId = do
  orionUrl <- asks orionUrl
  opts <- wOpts
  let url = orionUrl <> "/" <> entityId
  liftIO $ W.getWith opts url

postToOrion :: String -> MeterReadingRecord -> RIO App (W.Response BL.ByteString)
postToOrion entityId record = do
  orionUrl <- asks orionUrl
  opts' <- wOpts
  let opts = opts' & W.header "content-type" .~ ["application/json"]
  liftIO $ do
    putStrLn $ "POST " <> orionUrl
    W.postWith opts orionUrl (encode . object . makePostPayload entityId $ record)

patchOrion :: String -> MeterReadingRecord -> RIO App (W.Response BL.ByteString)
patchOrion entityId record = do
  orionUrl <- asks orionUrl
  service <- asks fiwareService
  opts' <- wOpts
  let opts = opts' & W.header "content-type" .~ ["application/json"]
  let url = orionUrl <> "/" <> entityId <> "/attrs"
  liftIO $ do
    putStrLn $ "PATCH " <> url
    W.customPayloadMethodWith "PATCH" opts url (encode . object . makePatchPayload entityId $ record)


makePostPayload :: KeyValue a => String -> MeterReadingRecord -> [a]
makePostPayload dataset record@MeterReadingRecord{..} =
  [ "id" .= asStr dataset
  , "type" .= asStr "HeaterEnergyConsumptionMeasurement"
  ] ++ makePatchPayload dataset record

makePatchPayload :: KeyValue a => String -> MeterReadingRecord -> [a]
makePatchPayload dataset MeterReadingRecord{..} =
  [ "dateMeasured" .= object [ "type" .= asStr "DateTime"
                             , "value" .= msjoDatum ]
  , "location" .= location dataset
  , "measuredValue" .=  object [ "type" .= asStr "Number"
                               , "value" .= msjoWert ]
  , "unit" .= object [ "type" .= asStr "Text"
                     , "value" .= asStr "kWh" ]
  ]
