{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoFieldSelectors #-}
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
import Data.String (IsString(..))
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


-- renderHeader :: Header -> Builder
-- renderHeader (Header m) = _








-- Keyworld Lines -----------------------------------------------------

renderKeywordLine :: Keyword -> Value -> Maybe Comment -> Builder
renderKeywordLine k v mc =
  let kv = renderKeywordValue k v
  in (addComment kv mc).builder
  where
    addComment kv Nothing = kv
    addComment kv (Just c) =
      let mx = 80 - kv.length
      in kv <> renderComment mx c




renderKeywordValue :: Keyword -> Value -> BuilderLine
renderKeywordValue k v = mconcat
  [ renderKeyword k
  , string "= "
  , renderValue v
  ]

renderKeyword :: Keyword -> BuilderLine
renderKeyword (Keyword k) = justifyLeft 8 $ string $ map toUpper $ take 8 $ unpack k

renderComment :: Int -> Comment -> BuilderLine
renderComment mx (Comment c) = string $ take mx $ " / " <> unpack c


-- must appear in byte thirty
renderValue :: Value -> BuilderLine
renderValue (Logic T) = justifyRight 30 "T"
renderValue (Logic F) = justifyRight 30 "F"
renderValue (Float f) = justifyRight 30 $ string $ map toUpper $ show f
renderValue (Integer n) = justifyRight 30 $ string $ show n
renderValue (String s) = string $ unpack s

justifyRight :: Int -> BuilderLine -> BuilderLine
justifyRight n b = spaces (n - b.length) <> b

justifyLeft :: Int -> BuilderLine -> BuilderLine
justifyLeft n b = b <> spaces (n - b.length)

spaces :: Int -> BuilderLine
spaces n = BuilderLine n $ mconcat $ replicate n $ charUtf8 ' '

string :: String -> BuilderLine
string s = BuilderLine (length s) (stringUtf8 s)

lineBuilder :: BuilderLine -> Builder
lineBuilder bl = bl.builder

data BuilderLine = BuilderLine { length :: Int, builder :: Builder }

instance IsString BuilderLine where
  fromString = string

instance Semigroup BuilderLine where
  BuilderLine l b <> BuilderLine l2 b2 = BuilderLine (l + l2) (b <> b2)

instance Monoid BuilderLine where
  mempty = BuilderLine 0 mempty


