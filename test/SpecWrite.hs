{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module SpecWrite where


import Data.Fits
import Data.Fits.Write
import Test
import Data.Fits.Read (getKeyword')
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Builder (Builder, toLazyByteString)
import Data.Text (pack)


tests :: Test ()
tests = do
  describe "gen header" testGenHeader
  describe "render header" testRenderHeader

testGenHeader :: Test ()
testGenHeader = do
  describe "mandatory" $ do
    let h = headerMandatory (Dimensions EightBitInt [3,2])

    it "main" $ do
      keywordEquals "SIMPLE" (Logic T) h
      keywordEquals "BITPIX" (Integer 8) h

    it "NAXIS" $ do
      keywordEquals "NAXIS" (Integer 2) h

    it "NAXISn" $ do
      keywordEquals (naxis 1) (Integer 3) h
      keywordEquals (naxis 2) (Integer 2) h

  where
    naxis n = Keyword $ "NAXIS" <> pack (show n)

    keywordEquals :: Keyword -> Value -> Header -> IO ()
    keywordEquals k v h = do
      vx <- getKeyword' k h
      vx @?= v

testRenderHeader :: Test ()
testRenderHeader = do
  describe "numbers" $ do
    it "int should justify" $ do
      run (renderValue (Integer 8)) @?= BL.pack (replicate n ' ' <> 8


  where
    run :: Builder -> BL.ByteString
    run = toLazyByteString


