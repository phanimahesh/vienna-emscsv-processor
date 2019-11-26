{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CSVParser
  ( MeterReadingRecord(..)
  , decodeCSV
  , DateTime(..)
  ) where

import           Data.Aeson (ToJSON, toJSON)
import qualified Data.ByteString.Lazy as BL
import           Data.Char (ord)
import qualified Data.Csv as Csv
import           Data.Time (parseTimeOrError, defaultTimeLocale, LocalTime)
import           Data.Vector (Vector)
import           GHC.Generics (Generic)

decodeCSV :: BL.ByteString -> Vector MeterReadingRecord
decodeCSV = either error id . Csv.decodeWith opts Csv.HasHeader
    where
      opts = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral $ ord ';' }

-- The fields should be in order of columns in the file
data EmsCsvRecord = EmsCsvRecord { compLevel :: Int
                                 , msjoDatum :: DateTime
                                 , zeitId :: Int
                                 , messId :: Int
                                 , msjoWert :: Double -- The only thing we need for now.
                                 , msjoInterval :: Int
                                 , msjoDgueltig :: Int
                                 , sterFlag :: Int
                                 , stkoFlag :: Int
                                 } deriving (Generic, Show)

instance Csv.FromRecord EmsCsvRecord

-- This contains fields we are actually interested in reading
data MeterReadingRecord = MeterReadingRecord { msjoDatum :: DateTime
                                             , msjoWert :: Double
                                             } deriving (Show)

-- Lets us parse MeterReadingRecords directly from CSV.
instance Csv.FromRecord MeterReadingRecord where
  parseRecord v = fromEmscsvRecord <$> Csv.parseRecord v
    where
      fromEmscsvRecord EmsCsvRecord{..} = MeterReadingRecord {..}
  
-- Newtype to handle special parsing and serialization for datetime
newtype DateTime = DateTime LocalTime deriving (Show,Eq)

instance Csv.FromField DateTime where
  parseField s = parseDateTime <$> Csv.parseField s
    where
    parseDateTime :: String -> DateTime
    parseDateTime = DateTime . parseTimeOrError True defaultTimeLocale "%d.%m.%Y %H:%M:%S"

instance ToJSON DateTime where
  toJSON (DateTime dt) = toJSON dt
