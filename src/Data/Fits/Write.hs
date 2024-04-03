{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}
module Data.Fits.Write where

import Data.Fits
import Data.Fits.Array
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.ByteString.Lazy (ByteString)
import Data.Map qualified as M
import Data.Text (pack, unpack, Text)
import Control.Monad (replicateM_)
import Data.Text qualified as T
import Data.ByteString.Builder
import Data.Char (toUpper)


-- | Encode HeaderDataUnits into a FITS file
--
-- >>> import qualified Data.ByteString.Lazy as BL
-- >>> BL.writeFile "myfile.fits" $ encodeHDUs hdus
-- encodeHDUs :: [HeaderDataUnit] -> BL.ByteString
-- encodeHDUs [] = error "encodeHDUs: []"
-- encodeHDUs hdus = _


-- | Creates an HDU from an ImageData and the provided extra Headers
imageHDU :: Header -> ImageData -> HeaderDataUnit
imageHDU h img =
  let _header = h <> headerMandatory img._imgDimensions
  in HeaderDataUnit
    { _header
    , _extension = Image
    , _dimensions = img._imgDimensions
    , _mainData = img._imgData
    }


headerMandatory :: Dimensions -> Header
headerMandatory dim = Header $ M.fromList $ core <> naxes dim._axes
  where
    core =
      [ ("SIMPLE", Logic T)
      , ("BITPIX", bitpix dim._bitpix)
      , ("NAXIS", Integer $ length dim._axes)
      ]

    naxes = zipWith naxis [1..]
      where
        naxis n a = (Keyword $ "NAXIS" <> pack (show n), Integer a)

    bitpix :: BitPixFormat -> Value
    bitpix EightBitInt = Integer 8
    bitpix SixteenBitInt = Integer 16
    bitpix ThirtyTwoBitInt = Integer 32
    bitpix SixtyFourBitInt = Integer 64
    bitpix ThirtyTwoBitFloat = Integer (-32)
    bitpix SixtyFourBitFloat = Integer (-64)



renderKeywordValue :: Keyword -> Value -> Builder
renderKeywordValue k v = mconcat
  [ renderKeyword k
  , stringUtf8 "= "
  , renderValue v
  ]

renderKeyword :: Keyword -> Builder
renderKeyword (Keyword k) = justifyLeft 8 (unpack k)

-- must appear in byte thirty
renderValue :: Value -> Builder
renderValue (Logic T) = justifyRight 30 "T"
renderValue (Logic F) = justifyRight 30 "F"
-- WARNING: probably incorrect
renderValue (Float n) = justifyRight 30 (show n)
renderValue (Integer n) = justifyRight 30 (show n)

justifyRight :: Int -> String -> Builder
justifyRight n s = spaces (n - length s) <> stringUtf8 s

justifyLeft :: Int -> String -> Builder
justifyLeft n s = stringUtf8 s <> spaces (n - length s)

spaces :: Int -> Builder
spaces n = mconcat (replicate n $ charUtf8 ' ')

renderFloat :: Float -> Builder
renderFloat f = stringUtf8 $ map toUpper $ show f



