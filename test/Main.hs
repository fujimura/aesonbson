{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Exception (evaluate)
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
    it "converts Int64 to Int64" $ property $ \(x :: Int64) ->
      ((bsonifyValue defaultAesonBsonOptions) . AESON.Number $ Scientific.scientific (fromIntegral x) 0)
        `shouldBe` BSON.Int64 x

  describe "Number conversion" $ do
    it "throws eror when the numbers are out of range with default option" $ do
      let tooBig   = succ $ fromIntegral (maxBound :: Int64) :: Integer
      let tooSmall = pred $ fromIntegral (minBound :: Int64) :: Integer
      let toScientific i = Scientific.scientific i 0

      evaluate ((bsonifyValue defaultAesonBsonOptions) . AESON.Number $ toScientific tooBig)
        `shouldThrow` (== OutOfBoundNumberInAeson (toScientific tooBig))

      evaluate ((bsonifyValue defaultAesonBsonOptions) . AESON.Number $ toScientific tooSmall )
        `shouldThrow` (== OutOfBoundNumberInAeson (toScientific tooSmall))
