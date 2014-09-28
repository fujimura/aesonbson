{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

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
  defaultAesonBsonOptions,
  AesonBsonOptions(..),
  OutOfBoundNumberInAeson(..)
) where

-- TODO Document the arbitrary choices in the Haddock.

import           Control.Exception (throw, Exception)
import           Data.Bson as BSON
import           Data.Aeson.Types as AESON
import qualified Data.Attoparsec.Number as Atto
import           Data.Int
import           Data.Monoid
import qualified Data.HashMap.Strict as HashMap (fromList, toList)
import qualified Data.Scientific as Scientific
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Typeable
import qualified Data.Vector as Vector (fromList, toList)

data AesonBsonOptions = AesonBsonOptions
  { numberConversion :: Scientific.Scientific -> BSON.Value
  }

data OutOfBoundNumberInAeson = OutOfBoundNumberInAeson Scientific.Scientific deriving (Eq,Show,Typeable)

instance Exception OutOfBoundNumberInAeson

defaultAesonBsonOptions :: AesonBsonOptions
defaultAesonBsonOptions = AesonBsonOptions defaultNumberConversion

defaultNumberConversion :: Scientific.Scientific -> BSON.Value
defaultNumberConversion n
     | exponent < 0                             = Float (Scientific.toRealFloat n :: Double)
     | int64MinBound <= n && n <= int64MaxBound = Int64 $ fromIntegral coefficient * 10 ^ exponent
     | otherwise                                = throw $ OutOfBoundNumberInAeson n
       where
         exponent       = Scientific.base10Exponent n
         coefficient    = Scientific.coefficient n
         int64MaxBound  = toScientific (maxBound :: Int64)
         int64MinBound  = toScientific (minBound :: Int64)
         toScientific i = Scientific.scientific (fromIntegral i :: Integer ) 0

-- | Converts a JSON value to BSON.
bsonifyValue :: AesonBsonOptions -> AESON.Value -> BSON.Value
bsonifyValue options (Object obj)        = Doc $ bsonify options obj
bsonifyValue options (AESON.Array array) = BSON.Array . map (bsonifyValue options) . Vector.toList $ array
bsonifyValue options (AESON.Number n)    = numberConversion options n
bsonifyValue _       (AESON.String str)  = BSON.String str
bsonifyValue _       (AESON.Bool b)      = BSON.Bool b
bsonifyValue _       (AESON.Null)        = BSON.Null

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
bsonify opts = map (\(t, v) -> t := (bsonifyValue opts) v) . HashMap.toList

-- | Converts a BSON document to an AESON object.
aesonify :: BSON.Document -> AESON.Object
aesonify = HashMap.fromList . map (\(l := v) -> (l, aesonifyValue v))
