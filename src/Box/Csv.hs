{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- | A csv process based on attoparsec and the box library
module Box.Csv
  ( CsvConfig (..),
    defaultCsvConfig,
    Header (..),
    rowEmitter,
    rowCommitter,
    runCsv,

    -- * parsers
    sep,
    field_,
    field,
    skipField_,
    skipField,
    int,
    int',
    A.double,
    double',
    fields,
    ints,
    doubles,
    day',
    tod',
    localtime',
  )
where

import Box
import Control.Monad
import qualified Data.Attoparsec.Text as A
import Data.Functor.Contravariant
import Data.Text (Text, unpack)
import qualified Data.Text as Text
import Data.Time
import GHC.Generics

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Box
-- >>> import Box.Csv
-- >>> import qualified Data.Text as Text
-- >>> import qualified Data.Attoparsec.Text as A

-- | csv file configuration
data CsvConfig = CsvConfig
  { -- | file name
    file :: FilePath,
    -- | field separator
    fsep :: Char,
    -- | nature of header row(s)
    header :: Header
  }
  deriving (Show, Generic, Eq)

-- | default csv file details
--
-- >>> defaultCsvConfig
-- CsvConfig {file = "./other/time_series_covid19_deaths_global_narrow.csv", fsep = ',', header = HasHXL}
--
-- test data from https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases
defaultCsvConfig :: CsvConfig
defaultCsvConfig =
  CsvConfig
    "./other/time_series_covid19_deaths_global_narrow.csv"
    ','
    HasHXL

-- | Type of header rows.  Note the modern propensity for multiple header rows.
data Header = HasHeader | HasHXL | NoHeader deriving (Show, Eq)

-- | attoparsec parse emitter which returns the original text on failure
parseE :: (Functor m) => A.Parser a -> Emitter m Text -> Emitter m (Either Text a)
parseE parser e = (\t -> either (const $ Left t) Right (A.parseOnly parser t)) <$> e

-- | A continuation emitter of parsed csv rows from a CsvConfig, returning the original text on failure
-- >>> emit <$|> rowEmitter defaultCsvConfig fields
-- Just (Right ["Province/State","Country/Region","Lat","Long","Date","Value","ISO 3166-1 Alpha 3-Codes","Region Code","Sub-region Code","Intermediate Region Code\r"])
rowEmitter :: CsvConfig -> (Char -> A.Parser a) -> CoEmitter IO (Either Text a)
rowEmitter cfg p = parseE (p (fsep cfg)) <$> fileE (file cfg)

-- | commits printed csv rows
--
-- >>> let testConfig = CsvConfig "./test/test.csv" ',' NoHeader
-- >>> let ctest = rowCommitter testConfig (fmap (Text.intercalate "," . fmap (Text.pack . show)))
--
-- >>> (\c -> commit c [[1..10::Int]]) <$|> ctest
-- True
--
-- >>> emit <$|> rowEmitter testConfig ints
-- Just (Right [1,2,3,4,5,6,7,8,9,10])
rowCommitter :: CsvConfig -> (a -> [Text]) -> CoCommitter IO a
rowCommitter cfg f = contramap (Text.intercalate (Text.singleton $ fsep cfg) . f) <$> fileWriteC (file cfg)

-- | Run a parser across all lines of a file.
--
-- >>> r1 <- runCsv defaultCsvConfig fields
-- >>> length r1
-- 42562
--
-- >>> length [x | (Left x) <- r1]
-- 0
--
-- >>> take 2 $ drop 2 [x | (Right x) <- r1]
-- [["","Afghanistan","33.0","65.0","2020-06-29","733","AFG","142","34","\r"],["","Afghanistan","33.0","65.0","2020-06-28","721","AFG","142","34","\r"]]
runCsv :: CsvConfig -> (Char -> A.Parser a) -> IO [Either Text a]
runCsv cfg p = toListM <$|> rowEmitter cfg p

-- * low-level generic csv parser helpers

-- | Most parsing and building routines implicity assume a character acting as a separator of fields, and newlines separating rows.
--
-- >>> A.parse (sep ',') ",ok"
-- Done "ok" ()
sep :: Char -> A.Parser ()
sep c = void (A.char c)

-- * single field parsers

-- | an unquoted field
-- Does not consume the separator token
--
-- >>> A.parse (field_ ',') "field,ok"
-- Done ",ok" "field"
field_ :: Char -> A.Parser Text
field_ c = A.takeWhile (/= c)

-- | an unquoted field
-- Consume the separator token
--
-- >>> A.parse (field ',') "field,ok"
-- Done "ok" "field"
field :: Char -> A.Parser Text
field c = A.takeWhile (/= c) <* A.char c

-- | skipping a field
--
-- >>> A.parse (skipField_ ',') "field,ok"
-- Done ",ok" ()
skipField_ :: Char -> A.Parser ()
skipField_ c = A.skipWhile (/= c)

-- | skipping a field
--
-- >>> A.parse (skipField ',') "field,ok"
-- Done "ok" ()
skipField :: Char -> A.Parser ()
skipField c = A.skipWhile (/= c) <* A.char ','

-- | int parser
--
-- >>> A.parse int "234,ok"
-- Done ",ok" 234
int :: A.Parser Int
int = A.decimal

-- | int parser, consumes separator
--
-- >>> A.parse (int' ',') "234,ok"
-- Done "ok" 234
int' :: Char -> A.Parser Int
int' c = A.decimal <* A.char c

-- | double parser, consumes separator
--
-- >>> A.parse (double' ',') "234.000,ok"
-- Done "ok" 234.0
double' :: Char -> A.Parser Double
double' c = A.double <* A.char c

-- | Day parser, consumes separator
--
-- >>> A.parse (day' ',') "2020-07-01,ok"
-- Done "ok" 2020-07-01
day' :: Char -> A.Parser Day
day' c = do
  d <- A.takeTill (== c)
  d' <- parseTimeM False defaultTimeLocale "%F" (unpack d)
  _ <- A.char c
  pure d'

-- | TimeOfDay parser, consumes separator
--
-- >>> A.parse (tod' ',') "23:52:05.221109,ok"
-- Done "ok" 23:52:05.221109
tod' :: Char -> A.Parser TimeOfDay
tod' c = do
  d <- A.takeTill (== c)
  d' <- parseTimeM False defaultTimeLocale "%T%Q" (unpack d)
  _ <- A.char c
  pure d'

-- | TimeOfDay parser, consumes separator
--
-- >>> A.parse (localtime' ',') "Jun 24 8:24AM,ok"
-- Done "ok" 2020-06-24 08:24:00
localtime' :: Char -> A.Parser LocalTime
localtime' c = do
  d <- A.takeTill (== c)
  d' <- parseTimeM False defaultTimeLocale "%Y %b %e %k:%M%p" (unpack ("2020 " <> d))
  _ <- A.char c
  pure d'

-- * Block list parsers

-- | Parser for a csv row of [Text].
--
-- >>> A.parseOnly (fields ',') "field1,field2\r"
-- Right ["field1","field2\r"]
fields :: Char -> A.Parser [Text]
fields c =
  field_ c `A.sepBy1` sep c

-- | parser for a csv row of [Double]
--
-- >>> A.parseOnly (doubles ',') "1,2,3"
-- Right [1.0,2.0,3.0]
doubles :: Char -> A.Parser [Double]
doubles c = A.double `A.sepBy1` sep c

-- | parser for a csv row of [Int]
--
-- >>> A.parseOnly (ints ',') "1,2,3"
-- Right [1,2,3]
ints :: Char -> A.Parser [Int]
ints c = A.signed A.decimal `A.sepBy1` sep c
