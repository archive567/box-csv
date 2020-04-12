{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Csv.Core
  ( sep,
    field,
    skipField,
    record,
    scis,
    doubles,
  )
where

import Data.Scientific
import NumHask.Prelude
import qualified Data.Attoparsec.Text as A

-- * low-level generic csv parser helpers

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XNoImplicitPrelude

-- | Most parsing and building routines implicity assume comma separated, and newlines separating rows.
--
-- >>> AC.parse (sep '\t') "\tok"
-- Done "ok" ()
sep :: Char -> A.Parser ()
sep c = void (A.char c)

-- | an unquoted field
-- Does not consume the separator token
-- >>> AC.parse (field ',') "field,ok"
-- Done ",ok" "field"
field :: Char -> A.Parser Text
field c = A.takeWhile (`notElem` [c])

-- | skipping a field
-- >>> AC.parse (skipField ',') "field,ok"
-- Done ",ok" ()
skipField :: Char -> A.Parser ()
skipField c = A.skipWhile (`notElem` [c])

-- | elements of the parsed list
-- >>> A.parse (record ',') "field1,field2\nok"
-- Done "ok" ["field1","field2"]
record :: Char -> A.Parser [Text]
record c = field c `A.sepBy1` sep c

-- | record parser for a csv row of all scientifics
-- >>> A.parse (scis ',') "1,2.2,3.3"
-- Done "" [1.0,2.2,3.3]
scis :: Char -> A.Parser [Scientific]
scis c = A.scientific `A.sepBy1` sep c

-- | record parser for a csv row of all scientifics
-- >>> A.parse (doubles ',') "1,2,3"
-- Done "" [1.0,2.0,3.0]
doubles :: Char -> A.Parser [Double]
doubles c = A.double `A.sepBy1` sep c

