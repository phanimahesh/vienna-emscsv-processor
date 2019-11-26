{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
  ( processAllEntries )
where

import           Control.Lens hiding ((.=))
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.Function ((&))
import           Data.List (stripPrefix, intercalate, sort)
import qualified Data.Map as M
import           Data.Maybe (fromJust, isJust)
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
handleStatusCodeException setId record (HttpExceptionRequest _req e) =
  case e of
    StatusCodeException res _ ->
      let code = res ^. ( W.responseStatus . W.statusCode )
      in
      case code of
        422 -> void $ do
          liftIO.putStrLn $ " - Got statuscode 422. Trying to patch"
          patchOrion setId record
        _ -> liftIO $ putStrLn $ "[WARN] Unexpected status: " ++ show code ++ " Ignoring."
    ConnectionFailure _ ->
      liftIO $ putStrLn "[WARN] Unable to connect to server. Ignoring."



