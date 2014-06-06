{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Aeson.Types as AESON
import           Data.Bson as BSON
import           Data.Int
import qualified Data.Text as T
import qualified Data.Scientific as Scientific

import           Data.AesonBson

main :: IO ()
main = hspec $ do
  describe "BSON -> JSON" $ do

    it "converts an ObjId to 24 digits hex" $ do
      -- https://github.com/nh2/aesonbson/pull/2
      let objid = ObjId (read "000000010000000000000001" :: ObjectId)
          AESON.String str = aesonifyValue objid
      str `shouldBe` "000000010000000000000001"
      T.length str `shouldBe` 24

  describe "JSON -> BSON" $ do
    it "converts decimals to float" $ do
      (bsonifyValue . AESON.Number $ Scientific.scientific 3 (-1))
        `shouldBe` BSON.Float 0.3

    it "converts Int64 max bound to Int64" $ do
      let x = maxBound :: Int64
      (bsonifyValue . AESON.Number $ Scientific.scientific (fromIntegral x) 0)
        `shouldBe` BSON.Int64 x

    it "converts Int64 min bound to Int64" $ do
      let x = minBound :: Int64
      (bsonifyValue . AESON.Number $ Scientific.scientific (fromIntegral x) 0)
        `shouldBe` BSON.Int64 x

    it "converts Int64 to Int64" $ property $ \(x :: Int64) ->
      (bsonifyValue . AESON.Number $ Scientific.scientific (fromIntegral x) 0)
        `shouldBe` BSON.Int64 x
