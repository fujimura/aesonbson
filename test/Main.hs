{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import           Test.Hspec
import           Test.QuickCheck

import           Data.Aeson.Types as AESON
import           Data.Bson as BSON
import           Data.Int
import qualified Data.Text as T
import qualified Data.Scientific as Scientific

import Control.Exception(evaluate)
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
    context "with default option" $ do
      it "converts decimals to float" $ do
        (bsonifyValue defaultAesonBsonOptions . AESON.Number $ Scientific.scientific 3 (-1))
          `shouldBe` BSON.Float 0.3

      it "converts Int64 max bound to Int64" $ do
        let x = maxBound :: Int64
        (bsonifyValue defaultAesonBsonOptions . AESON.Number $ Scientific.scientific (fromIntegral x) 0)
          `shouldBe` BSON.Int64 x

      it "converts Int64 min bound to Int64" $ do
        let x = minBound :: Int64
        (bsonifyValue defaultAesonBsonOptions . AESON.Number $ Scientific.scientific (fromIntegral x) 0)
          `shouldBe` BSON.Int64 x

      it "converts Int64 to Int64" $ property $ \(x :: Int64) ->
        (bsonifyValue defaultAesonBsonOptions . AESON.Number $ Scientific.scientific (fromIntegral x) 0)
          `shouldBe` BSON.Int64 x

      it "converts number which is too big for Int64 to 0" $ do
        let x = succ . succ $ fromIntegral (maxBound :: Int64) :: Integer
        print $ show x
        print $ Scientific.scientific x 0
        print $ AESON.Number $ Scientific.scientific x 0
        (bsonifyValue defaultAesonBsonOptions . AESON.Number $ Scientific.scientific x 0)
          `shouldBe` BSON.Int64 0

      it "raises error when given number is too small for Int64" $ do
        let x = pred $ fromIntegral (minBound :: Int64) :: Integer
        (bsonifyValue defaultAesonBsonOptions . AESON.Number $ Scientific.scientific x 0)
          `shouldBe` BSON.Int64 0
