{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
--
-- csv data type
module Data.Csv
  ( CsvConfig(..),
    defaultCsvConfig,
    file,
    Header(..),
    rowEmitter,
    rowCommitter,
    runCsv,
    sep,
    field,
    skipField,
    texts,
    scis,
    doubles,
    ints,
  ) where

import Box
import Control.Lens
import qualified Data.Attoparsec.Text as A
import Data.Generics.Labels ()
import qualified Data.Text as Text
import NumHask.Prelude
import Data.Scientific

data CsvConfig
  = CsvConfig
      { -- | data set name
        name :: Text,
        -- | file suffix
        suffix :: Text,
        -- | local directory
        dir :: Text,
        -- | separator
        csep :: Char,
        -- | first row is a header row
        header :: Header
      }
  deriving (Show, Generic, Eq)

-- | CsvConfig "Hill_Valley_with_noise" ".csv" "./other" '\t' HasHeader
defaultCsvConfig :: CsvConfig
defaultCsvConfig =
  CsvConfig
    "Hill_Valley_with_noise"
    ".csv"
    "./other"
    '\t'
    HasHeader

-- | filepath for teh config.
file :: CsvConfig -> FilePath
file cfg =
  cfg ^. #dir
    <> "/"
    <> cfg ^. #name
    <> cfg ^. #suffix
      & Text.unpack

-- | does the csv have a header row?
data Header = HasHeader | NoHeader deriving (Show, Eq)

-- | emits parsed csv rows
rowEmitter :: CsvConfig -> (Char -> A.Parser a) -> Cont IO (Emitter IO (Either Text a))
rowEmitter cfg p = eParse (p (view #csep cfg)) <$> fileEmitter (file cfg)

-- | emits parsed csv rows
-- rowCommitter :: CsvConfig -> Cont IO (Committer IO [Text])
rowCommitter :: CsvConfig -> (a -> [Text]) -> Cont IO (Committer IO a)
rowCommitter cfg f = contramap (Text.intercalate (Text.singleton $ view #csep cfg) . f) <$> fileCommitter (file cfg)

-- | Run a parser across all lines of a file.
--
-- >>> xss <- rights <$> runCsv defaultCsvConfig doubles
--
-- >>> length xss
-- 1212
--
-- >>> all ((==101) . length) xss
-- True
--
-- >>> take 2 xss
-- [[39.02,36.49,38.2,38.85,39.38,39.74,37.02,39.53,38.81,38.79,37.65,39.34,38.55,39.03,37.21,36.32,37.81,38.95,36.7,39.72,37.06,37.29,36.43,36.53,36.19,38.17,37.3,36.15,36.68,36.7,36.68,36.99,38.92,37.25,37.47,36.32,35.75,35.68,34.66,34.26,35.62,36.6,34.78,34.67,34.3,33.4,31.4,31.75,31.75,32.84,33.76,35.74,34.01,33.91,36.88,34.41,35.52,36.94,36.95,35.57,38.02,37.32,39.05,37.97,37.01,38.98,38.83,38.87,38.03,38.4,38.25,38.61,36.23,37.81,37.98,38.58,38.96,38.97,39.08,38.79,38.79,36.31,36.59,38.19,37.95,39.63,39.27,37.19,37.13,37.47,37.57,36.62,36.92,38.8,38.52,38.07,36.73,39.46,37.5,39.1,0.0],[1.83,1.71,1.77,1.77,1.68,1.78,1.8,1.7,1.75,1.78,1.86,1.76,1.81,1.86,1.74,1.78,1.81,2.02,2.0,2.01,2.0,2.06,2.0,1.93,1.88,1.85,1.89,1.83,1.76,1.83,1.81,1.81,1.78,1.85,1.86,1.73,1.79,1.81,1.85,1.71,1.71,1.71,1.84,1.76,1.73,1.83,1.68,1.73,1.76,1.77,1.72,1.75,1.66,1.76,1.77,1.78,1.63,1.72,1.66,1.67,1.74,1.65,1.74,1.79,1.69,1.76,1.74,1.82,1.78,1.65,1.65,1.82,1.71,1.83,1.72,1.63,1.77,1.69,1.81,1.74,1.7,1.72,1.74,1.72,1.74,1.71,1.7,1.83,1.79,1.78,1.71,1.8,1.79,1.77,1.74,1.74,1.8,1.78,1.75,1.69,1.0]]
runCsv :: CsvConfig -> (Char -> A.Parser a) -> IO [Either Text a]
runCsv cfg p = with (rowEmitter cfg p) toList'

-- Convert an emitter to a list.
toList' :: Emitter IO a -> IO [a]
toList' e = go []
  where
    go xs = do
      r <- emit e
      case r of
        Nothing -> pure (reverse xs)
        Just x -> go (x : xs)

-- * low-level generic csv parser helpers

-- $setup
-- >>> :set -XOverloadedStrings

-- | Most parsing and building routines implicity assume comma separated, and newlines separating rows.
--
-- >>> A.parse (sep ',') ",ok"
-- Done "ok" ()
sep :: Char -> A.Parser ()
sep c = void (A.char c)

-- | an unquoted field
-- Does not consume the separator token
-- >>> A.parse (field ',') "field,ok"
-- Done ",ok" "field"
field :: Char -> A.Parser Text
field c = A.takeWhile (`notElem` [c])

-- | skipping a field
-- >>> A.parse (skipField ',') "field,ok"
-- Done ",ok" ()
skipField :: Char -> A.Parser ()
skipField c = A.skipWhile (`notElem` [c])


-- * Block list parsers

-- | parser for a csv row of [Text]
-- >>> A.parseOnly (texts ',') "field1,field2"
-- Right ["field1","field2"]
texts :: Char -> A.Parser [Text]
texts c = field c `A.sepBy1` sep c

-- | parser for a csv row of [Scientific]
-- >>> A.parseOnly (scis ',') "1,2.2,3.3"
-- Right [1.0,2.2,3.3]
scis :: Char -> A.Parser [Scientific]
scis c = A.scientific `A.sepBy1` sep c

-- | parser for a csv row of [Double]
-- >>> A.parseOnly (doubles ',') "1,2,3"
-- Right [1.0,2.0,3.0]
doubles :: Char -> A.Parser [Double]
doubles c = A.double `A.sepBy1` sep c

-- | parser for a csv row of [Int]
-- >>> A.parseOnly (ints ',') "1,2,3"
-- Right [1,2,3]
ints :: Char -> A.Parser [Int]
ints c = A.signed A.decimal `A.sepBy1` sep c

