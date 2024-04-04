{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module SpecWrite where


import Data.Fits
import Data.Fits.Write
import Test
import Data.Fits.Read (getKeyword')
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as C8
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
  describe "renderValue" $ do
    it "int should right justify" $ do
      runValue (Integer 8) @?= just30 "8"

    it "float should right justify" $ do
      runValue (Float 3.2) @?= just30 "3.2"

    it "float should exponent uppercase" $ do
      runValue (Float 6.00000000000001e-16) @?= just30 "6.00000000000001E-16"

    it "logic should right justify" $ do
      runValue (Logic T) @?= just30 "T"

    it "string" $ do
      runValue (String "Hello World") @?= "Hello World"

    -- TODO: does it matter if e-06 vs e-6? We output e-6.

  describe "renderKeyword" $ do
    it "should left justify" $ do
      runLine (renderKeyword "BITPIX") @?= "BITPIX  "

    it "should truncate" $ do
      runLine (renderKeyword "REALLYLONG") @?= "REALLYLO"

    it "should uppercase" $ do
      runLine (renderKeyword "lower") @?= "LOWER   "

  describe "renderKeywordValue" $ do
    it "should render space" $ do
      runLine (renderKeywordValue "SIMPLE" (Logic T)) @?= ("SIMPLE  = "  <> just30 "T")

    it "should butt against equals" $ do
      runLine (renderKeywordValue "WHATEVER" (Integer 10)) @?= ("WHATEVER= "  <> just30 "10")

    it "should string" $ do
      runLine (renderKeywordValue "WHATEVER" (String "dude")) @?= "WHATEVER= dude"

  describe "renderComment" $ do
    it "should render comment" $ do
      runLine (renderComment 80 (Comment "Hello World")) @?= " / Hello World"

    it "should truncate comment" $ do
      runLine (renderComment 10 (Comment "Hello World")) @?= " / Hello W"

    it "should render comment in line" $ do
      run (renderKeywordLine "SIMPLE" (Logic T) (Just "Comment")) @?= "SIMPLE  = "  <> just30 "T" <> " / Comment"

    it "should render no comment" $ do
      run (renderKeywordLine "SIMPLE" (Logic T) Nothing) @?= "SIMPLE  = "  <> just30 "T"

    it "should truncate whole line" $ do
      run (renderKeywordLine "SIMPLE" (Logic T) Nothing) @?= "SIMPLE  = "  <> just30 "T"


  where
    runLine :: BuilderLine -> String
    runLine = run . lineBuilder

    run :: Builder -> String
    run = C8.unpack . toLazyByteString

    runValue :: Value -> String
    runValue = runLine . renderValue

    just30 :: String -> String
    just30 s = spaces (30 - length s) <> s

    spaces :: Int -> String
    spaces n = replicate n ' '


