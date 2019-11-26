{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
  ( processAllEntries )
where

import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Lens (key, _String, _Double)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.Function ((&))
import           Data.List (stripPrefix, intercalate, sort)
import qualified Data.Map as M
import           Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import           Data.Time
import qualified Data.Vector as V
import           Network.HTTP.Client
import qualified Network.Wreq as W
import           Network.Wreq.Types

import RIO hiding ((^.))

import Config
import CSVParser
import Utils
import Fiware
import Emscsv

processAllEntries :: RIO App ()
processAllEntries = do
  links <- getLinks
  -- links are now an array of file links
  prefix <- asks csvUrlPrefix
  let datasets' :: [(Maybe String, [String])]
      datasets' = filter (\(k,v) -> isJust k) $ map (\l -> (datasetNameFromLink prefix l, [l])) links
      datasets :: Map String [String]
      datasets = M.map sort $ M.mapKeys fromJust $ M.fromListWith (++) datasets'
  itraverse_ processLatestEntry datasets

processLatestEntry :: String -> [String] -> RIO App ()
processLatestEntry setId links = do
  latestRecord <- V.last <$> (getAndDecodeCsv . last $ links)
  liftIO $ do
    putStrLn ""
    putStrLn $ "Processing dataset " ++ setId
    putStrLn $ "Latest record: " ++ show latestRecord
  let handlers = Handler <$> [handleStatusCodeException setId latestRecord]
  void (postToOrion setId latestRecord) `catches` handlers
  liftIO . putStrLn $ "[OK] Processing complete"
  return ()

handleStatusCodeException
  :: String -> MeterReadingRecord -> HttpException -> RIO App ()
handleStatusCodeException setId record@MeterReadingRecord{..} (HttpExceptionRequest _req e) =
  case e of
    StatusCodeException res _ ->
      let code = res ^. ( W.responseStatus . W.statusCode )
      in
      case code of
        422 -> void $ do
          liftIO.putStrLn $ " - Got statuscode 422. Inspecting existing entry"
          r <- getFromOrion setId
          let dateT :: Text
              -- taking 19 gives 2019-11-24T05:15:00 from 2019-11-24T05:15:00.00Z
              dateT = T.take 19 $ r ^. W.responseBody . key "dateMeasured" . key "value" . _String
              date = DateTime $ parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%S" $ T.unpack dateT
              datesMatch = date == msjoDatum

              measuredValue :: Double
              measuredValue = fromMaybe 0.0 $ r ^? W.responseBody . key "measuredValue" . key "value" . _Double
              valuesMatch = measuredValue == msjoWert

          if datesMatch && valuesMatch
             then liftIO . putStrLn $ " - Existing entry matches. Ignoring"
             else void $ patchOrion setId record
        _ -> liftIO $ putStrLn $ "[WARN] Unexpected status: " ++ show code ++ " Ignoring."
    ConnectionFailure _ ->
      liftIO $ putStrLn "[WARN] Unable to connect to server. Ignoring."
    e ->
      liftIO $ do
        putStrLn "[WARN] Unexpected error encountered"
        print e
        putStrLn "Ignoring and attempting to proceed"



