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
  HeaderDataUnit
    { _header = h
    , _extension = Image
    , _dimensions = img._imgDimensions
    , _mainData = img._imgData
    }


-- | Execute a BuilderBlock and create a bytestring
runRender :: BuilderBlock -> BL.ByteString
runRender bb = toLazyByteString bb.builder

renderHDU :: HeaderDataUnit -> BuilderBlock
renderHDU hdu = renderHeader hdu <> renderData hdu._mainData

renderData :: BL.ByteString -> BuilderBlock
renderData s = fillBlock $ BuilderBlock (fromIntegral $ BL.length s) $ lazyByteString s

renderHeader :: HeaderDataUnit -> BuilderBlock
renderHeader hdu = fillBlock $ mconcat 
  [ renderHDUType hdu._extension
  -- what if I don't set these. They are optional. Not sure if astro.py will be angry. I'm not sure they have very much valeu
  -- TODO: DATASUM - update first
  -- TODO: CHECKSUM - update very last
  , renderKeywordLine "CHECKSUM" (String "TODO") Nothing
  , renderKeywordLine "DATASUM" (String "TODO") Nothing
  , renderOtherKeywords hdu._header
  , renderEnd
  ]
  where
    renderHDUType Primary = renderPrimaryHeader hdu._dimensions
    renderHDUType Image = renderImageHeader hdu._dimensions
    renderHDUType _ = ""

    renderEnd = pad 80 "END"

renderImageHeader :: Dimensions -> BuilderBlock
renderImageHeader dim = mconcat
  [ renderKeywordLine "XTENSION" (String "IMAGE") (Just "Image Extension")
  , renderImageKeywords dim
  ]

renderPrimaryHeader :: Dimensions -> BuilderBlock
renderPrimaryHeader dim = mconcat
  [ renderKeywordLine "SIMPLE" (Logic T) (Just "Conforms to the FITS standard")
  , renderImageKeywords dim
  , renderKeywordLine "EXTEND" (Logic T) Nothing
  ]

renderImageKeywords :: Dimensions -> BuilderBlock
renderImageKeywords dim = mconcat
  [ bitpix
  , naxis_
  , naxes dim._axes
  ]
  where
    bitpix = renderKeywordLine "BITPIX" (Integer $ bitpixCode dim._bitpix) (Just "array data type")
    naxis_ = renderKeywordLine "NAXIS" (Integer $ length dim._axes) Nothing
    naxes as = mconcat $ zipWith naxisN [1..] as
    naxisN n a =
      renderKeywordLine (Keyword $ "NAXIS" <> pack (show n)) (Integer a) Nothing


-- | 'Header' should contain only extra keywords. The system will generate all mandatory keywords
renderOtherKeywords :: Header -> BuilderBlock
renderOtherKeywords (Header ks) =
  mconcat $ map (\(k, v) -> renderKeywordLine k v Nothing) ks

-- | Fill out the header or data block to the nearest 2880 bytes
fillBlock :: BuilderBlock -> BuilderBlock
fillBlock b =
  let rem = hduBlockSize - b.length `mod` hduBlockSize
  in b <> extraSpaces rem
  where
    extraSpaces n
      | n == hduBlockSize = mempty
      | otherwise = spaces n



bitpixCode :: BitPixFormat -> Int
bitpixCode EightBitInt = 8
bitpixCode SixteenBitInt = 16
bitpixCode ThirtyTwoBitInt = 32
bitpixCode SixtyFourBitInt = 64
bitpixCode ThirtyTwoBitFloat = -32
bitpixCode SixtyFourBitFloat = -64














-- Keyword Lines -----------------------------------------------------

renderKeywordLine :: Keyword -> Value -> Maybe Comment -> BuilderBlock
renderKeywordLine k v mc =
  let kv = renderKeywordValue k v
  in pad 80 $ addComment kv mc
  where
    addComment kv Nothing = kv
    addComment kv (Just c) =
      let mx = 80 - kv.length
      in kv <> renderComment mx c

renderKeywordValue :: Keyword -> Value -> BuilderBlock
renderKeywordValue k v = mconcat
  [ renderKeyword k
  , string "= "
  , pad 30 $ renderValue v
  ]

renderKeyword :: Keyword -> BuilderBlock
renderKeyword (Keyword k) = pad 8 $ string $ map toUpper $ take 8 $ unpack k

renderComment :: Int -> Comment -> BuilderBlock
renderComment mx (Comment c) = string $ take mx $ " / " <> unpack c

renderValue :: Value -> BuilderBlock
renderValue (Logic T) = justify 30 "T"
renderValue (Logic F) = justify 30 "F"
renderValue (Float f) = justify 30 $ string $ map toUpper $ show f
renderValue (Integer n) = justify 30 $ string $ show n
renderValue (String s) = string $ "'" <> unpack s <> "'"




-- Builder Block ---------------------------------------------------------

-- | We need a builder that keeps track of its length so we can pad things
data BuilderBlock = BuilderBlock { length :: Int, builder :: Builder }

instance IsString BuilderBlock where
  fromString = string

instance Semigroup BuilderBlock where
  BuilderBlock l b <> BuilderBlock l2 b2 = BuilderBlock (l + l2) (b <> b2)

instance Monoid BuilderBlock where
  mempty = BuilderBlock 0 mempty


justify :: Int -> BuilderBlock -> BuilderBlock
justify n b = spaces (n - b.length) <> b

pad :: Int -> BuilderBlock -> BuilderBlock
pad n b = b <> spaces (n - b.length)

spaces :: Int -> BuilderBlock
spaces n = BuilderBlock n $ mconcat $ replicate n $ charUtf8 ' '

string :: String -> BuilderBlock
string s = BuilderBlock (length s) (stringUtf8 s)
