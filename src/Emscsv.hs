{-# LANGUAGE OverloadedStrings #-}
module Emscsv
  (datasetNameFromLink
  , getAndDecodeCsv
  , getLinks
  ) where


import           Control.Lens hiding ((.=))
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Default
import           Data.List (stripPrefix, intercalate, sort)
import           Data.List.Split
import           Data.Maybe (fromJust, isJust)
import qualified Data.Vector as V
import           Data.X509.CertificateStore (readCertificateStore)
import           Network.Connection (TLSSettings(..), SockSettings)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (mkManagerSettings)
import qualified Network.TLS as TLS
import           Network.TLS.Extra.Cipher
import qualified Network.Wreq as W
import           Network.Wreq.Types
import           Text.HandsomeSoup
import           Text.XML.HXT.Core (runX, (>>>))
import RIO hiding ((^.))

import Config
import CSVParser
import Utils

mkMngr :: RIO App Manager
mkMngr = do
  crtStoreFile <- asks certificateStore
  crtFile <- asks certificateCrt
  keyFile <- asks certificateKey
  hostName <- asks managerHostName
  liftIO $ mkMngr' crtStoreFile crtFile keyFile hostName

mkMngr' :: FilePath -> FilePath -> FilePath -> String -> IO Manager
mkMngr' crtStoreFile crtFile keyFile hostName = do
  Just store <- readCertificateStore crtStoreFile
  creds <- either error Just `fmap` TLS.credentialLoadX509 crtFile keyFile
  let onCertificateRequest (_certTypes, _maybeAlgos, _dns) = return creds
      clientHooks :: TLS.ClientHooks
      clientHooks = def { TLS.onCertificateRequest = onCertificateRequest }
      -- clientHooks = def { TLS.onCertificateRequest = \_ -> return Nothing }
      clientParams :: TLS.ClientParams
      clientParams = (TLS.defaultParamsClient hostName "")
        { TLS.clientHooks = clientHooks
        , TLS.clientSupported = def { TLS.supportedCiphers = ciphersuite_default }
        , TLS.clientShared = def { TLS.sharedCAStore = store }
        }
      tlsSettings = TLSSettings clientParams
      sockSettings :: Maybe SockSettings
      sockSettings = Nothing
  newManager $ mkManagerSettings tlsSettings sockSettings

getAndDecodeCsv :: String -> RIO App (V.Vector MeterReadingRecord)
getAndDecodeCsv f = do
  baseUrl <- asks csvBaseUrl
  rCsv <- getWithAuth (baseUrl ++ f)
  let contents = rCsv ^. W.responseBody
  return $ decodeCSV contents


-- assumes everything starts with the prefix. crashes otherwise. but fine for us.
getFileName :: String -> String -> String
getFileName prefix url = fromJust $ stripPrefix prefix url

getNameComponents :: String -> [String]
getNameComponents = wordsBy (=='_')

datasetName :: String -> Maybe String
datasetName = fmap (intercalate "_") <$> components
  where
    components l = case take 3 l of
                     "Enk" -> Just . take 3 . getNameComponents $ l
                     "Lor" -> Just . take 4 . getNameComponents $ l
                     "000" -> Just . drop 2 . take 5 . getNameComponents $ l 
                     _ -> Nothing

datasetNameFromLink :: String -> String -> Maybe String
datasetNameFromLink p = datasetName . getFileName p 

getLinks :: RIO App [String]
getLinks = do
  emscsvURL <- asks csvIndexUrl
  -- throws unless status is successful. Not ideal, but works
  rIndex <- getWithAuth emscsvURL
  let doc = parseHtml $ BL.unpack $ rIndex ^. W.responseBody
      links' = doc >>> css "a" ! "href"
  links <- liftIO.runX $ links'
  -- first link is useless. discard it.
  return $ tail links

getWithAuth :: String -> RIO App (W.Response BL.ByteString)
getWithAuth url = do
  mgr <- mkMngr
  let opts :: W.Options
      opts = W.defaults { manager = Right mgr }
  liftIO $ W.getWith opts url
