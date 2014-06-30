{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- | Convert JSON to BSON and the other way around.
--
-- Note that BSON has more data types than JSON,
-- so some BSON to JSON conversions are not bijective and somewhat arbitrary.
--
-- This means that for some BSON objects:
--
-- >bsonify . aesonify /= id
-- >bsonifyValue . aesonifyValue /= id
--
-- We tried to choose sensible translations on those cases.
module Data.AesonBson (
  aesonify, aesonifyValue,
  bsonify, bsonifyValue,
  defaultAesonBsonOptions, AesonBsonOptions(..)
) where

-- TODO Document the arbitrary choices in the Haddock.

import           Data.Bson as BSON
import           Data.Aeson.Types as AESON
import qualified Data.Attoparsec.Number as Atto
import           Data.Int
import           Data.Monoid
import qualified Data.HashMap.Strict as HashMap (fromList, toList)
import qualified Data.Scientific as Scientific
import           Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as Vector (fromList, toList)

data AesonBsonOptions = AesonBsonOptions
  { numberConversion :: Scientific -> BSON.Value -- ^ how to convert a JSON number to a BSON number. This options is required since number in JSON and BSON don't have same type.
  }

-- | Default option. This will throw error if a number which is out of
-- Int64 or Double.
defaultAesonBsonOptions :: AesonBsonOptions
defaultAesonBsonOptions = AesonBsonOptions { numberConversion = coerceToBoundary }
  where
    coerceToBoundary n = case (Scientific.floatingOrInteger n :: Either Double Int64) of
                           Left r -> Float r
                           Right i -> Int64 i

-- | Converts a JSON value to BSON.
bsonifyValue :: AesonBsonOptions -> AESON.Value -> BSON.Value
bsonifyValue opts (Object obj) = Doc $ bsonify opts obj
bsonifyValue opts (AESON.Array array) = BSON.Array . map (bsonifyValue opts) . Vector.toList $ array
bsonifyValue _ (AESON.String str) = BSON.String str
bsonifyValue ops (AESON.Number n) = numberConversion ops n
bsonifyValue _ (AESON.Bool b) = BSON.Bool b
bsonifyValue _ (AESON.Null) = BSON.Null

-- | Converts a BSON value to JSON.
aesonifyValue :: BSON.Value -> AESON.Value
aesonifyValue (Float f) = toJSON f
aesonifyValue (BSON.String s) = toJSON s
aesonifyValue (Doc doc) = Object $ aesonify doc
aesonifyValue (BSON.Array list) = AESON.Array . Vector.fromList $ map aesonifyValue list
aesonifyValue (Bin (Binary binary)) = toJSON $ T.decodeUtf8 binary
aesonifyValue (Fun (Function function)) = toJSON $ T.decodeUtf8 function
aesonifyValue (Uuid (UUID uuid)) = toJSON $ T.decodeUtf8 uuid
aesonifyValue (Md5 (MD5 md5)) = toJSON $ T.decodeUtf8 md5
aesonifyValue (UserDef (UserDefined userdef)) = toJSON $ T.decodeUtf8 userdef
aesonifyValue (ObjId oid) = toJSON $ show oid -- Relies on bson to show the OID as 24 digit hex.
                                              -- It would be better if BSON exposed a non-show function for this,
                                              -- preferably a fast one.
aesonifyValue (BSON.Bool bool) = toJSON bool
aesonifyValue (UTC utc) = toJSON utc
aesonifyValue (BSON.Null) = AESON.Null
aesonifyValue (RegEx (Regex pattern mods)) = toJSON $ mconcat ["/", pattern, "/", mods]
aesonifyValue (JavaScr (Javascript env code)) = object [ "environment" .= aesonify env
                                                       , "code" .= code ]
aesonifyValue (Sym (Symbol sym)) = toJSON sym
aesonifyValue (Int32 int32) = toJSON int32
aesonifyValue (Int64 int64) = toJSON int64
aesonifyValue (Stamp (MongoStamp int64)) = toJSON int64
aesonifyValue (MinMax mm) = case mm of { MinKey -> toJSON (-1 :: Int)
                                       ; MaxKey -> toJSON (1 :: Int)}


-- | Converts an AESON object to a BSON document.
bsonify :: AesonBsonOptions -> AESON.Object -> BSON.Document
bsonify opts = map (\(t, v) -> t := bsonifyValue opts v) . HashMap.toList

-- | Converts a BSON document to an AESON object.
aesonify :: BSON.Document -> AESON.Object
aesonify = HashMap.fromList . map (\(l := v) -> (l, aesonifyValue v))
