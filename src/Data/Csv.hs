{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
--
-- csv data type
--
module Data.Csv where

import Data.Csv.Core
import NumHask.Prelude
import Control.Lens
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Generics.Labels ()
import Box
import qualified Control.Foldl as L
-- import Control.Category (id)
import qualified Data.Attoparsec.Text as A
-- import qualified Control.Exception as E

data CsvConfig
  = CsvConfig
      { -- | data set name
        name :: Text,
        -- | file suffix
        suffix :: Text,
        -- | local directory
        dir :: Text,
        -- | separator
        csep :: Char
      }
  deriving (Show, Generic, Eq)

defaultCsvConfig :: CsvConfig
defaultCsvConfig =
  CsvConfig
    "Hill_Valley_with_noise"
    ".csv"
    "./other"
    '\t'

file :: CsvConfig -> FilePath
file cfg =
  cfg ^. #dir
    <> "/"
    <> cfg ^. #name
    <> cfg ^. #suffix
      & Text.unpack

tsep :: CsvConfig -> Text
tsep cfg = cfg ^. #csep & Text.singleton

fileLines :: Handle -> Emitter IO Text
fileLines h = Emitter $ do
  l :: (Either IOException Text) <- try (Text.hGetLine h)
  pure $ case l of
    Left _ -> Nothing
    Right a -> bool (Just a) Nothing (a=="")

takeE :: Int -> Emitter IO a -> IO [a]
takeE n e = go [] 0
  where
    go xs c = do
      r <- emit e
      case r of
        Nothing -> pure (reverse xs)
        Just x -> bool (pure (reverse xs)) (go (x:xs) (c+1)) (c<n)

unsafeTakeE :: Emitter IO a -> IO [a]
unsafeTakeE e = go []
  where
    go xs = do
      r <- emit e
      case r of
        Nothing -> pure (reverse xs)
        Just x -> go (x:xs)

-- | default stats
-- >>> xss <- rights <$> defaultXs 10000
defaultXs :: IO [Either Text [Double]]
defaultXs = withFile (file defaultCsvConfig) ReadMode (\h -> unsafeTakeE (eParse (doubles '\t') $ fileLines h))


-- | Run a parser across all lines of a file.
--
-- >>> xss <- rights <$> rows "./other/Hill_Valley_with_noise.csv" (doubles '\t')
--
-- >>> length xss
-- 1212
--
-- >>> all ((==101) . length) xss
-- True
--
rows :: FilePath -> A.Parser a -> IO [Either Text a]
rows fp p = withFile fp ReadMode (\h -> unsafeTakeE (eParse p $ fileLines h))

-- | compression helper for a list of indexes
data Skippy = Skip Int | Retain Int deriving (Show)

-- | does the csv have a header row?
data Header = HasHeader | NoHeader deriving (Show, Eq)

-- | convert a list of column indexes to a Skippy
-- >>> toSkips (20, [3,4,12])
-- [Skip 3,Retain 2,Skip 7,Retain 1,Skip 7]
toSkips :: (Int, [Int]) -> [Skippy]
toSkips (n, is) = L.fold (L.Fold step ([], (0, 0)) done) is
  where
    step (f, (r, x)) a
      | a == x = (f, (r + 1, a + 1))
      | r == 0 = (Skip (a - x) : f, (1, a + 1))
      | otherwise = (Skip (a - x) : Retain r : f, (1, a + 1))
    done (f, (r, x))
      | x > n = reverse f
      | x == n && r == 0 = reverse f
      | r == 0 = reverse $ Skip (n - x) : f
      | n == x = reverse $ Retain r : f
      | otherwise = reverse $ Skip (n - x) : Retain r : f

-- | convert a list of Skippys to list of column indexes
-- >>> fromSkips [Skip 3,Retain 2,Skip 7,Retain 1,Skip 7]
-- (20,[3,4,12])
fromSkips :: [Skippy] -> (Int, [Int])
fromSkips = L.fold (L.Fold step (0, []) identity)
  where
    step (x, is) (Skip n) = (x + n, is)
    step (x, is) (Retain n) = (x + n, is <> ((x +) <$> take n [0 ..]))

-- | parse specific columns
-- >>> A.parse (cols (const double) 5 [0,3,4] ',') "0,1,2,3,4,5\n"
-- Done "5\n" [0.0,3.0,4.0]
cols :: (Char -> A.Parser a) -> Int -> [Int] -> (Char -> A.Parser [a])
cols p n is = L.fold (L.Fold step (const $ pure []) identity) (toSkips (n, is))
  where
    step x (Retain a) = \c -> (<>) <$> x c <*> mp a c
    step x (Skip a) = \c -> x c <* skipFields a c
    mp m c = replicateM m (p c <* sep c)
    skipFields m c = replicateM_ m (skipField c <* sep c)

