{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
-- you'll have to export all the things if you want it to be useful...
module Data.Fits.Array
  ( 
  -- * Encoding Images
    decodeImage
  , encodeImage
  -- * Encoding as ByteStrings
  , decodeArray
  , encodeArray
  -- * Handling Axes
  , totalPix
  , AxesIndex(..)
  , RowMajorAxes(..)
  , rowMajorAxes
  , getAxesVector
  , runGetThrow
  -- * Binary Encoding
  , GetPix(..)
  , PutPix(..)
  , PutArray(..)
  -- * Exports from Data.Massiv.Array
  , Array
  , Ix1, Ix2, Ix3, Ix4, Ix5
  ) where

import Control.Monad (replicateM)
import qualified Data.Fits as Fits
import Data.Int
import Data.Proxy
import Data.Fits (BitPixFormat(..), HeaderDataUnit(..), Axes(..), Axis, Header, Dimensions(..), Extension(..), ImageData(..))
import qualified Data.List as L
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary.Builder (Builder, toLazyByteString)
import Data.Binary as Binary (Binary(..))
import qualified Data.ByteString.Lazy as BL
import Data.Massiv.Array as A hiding (isEmpty, product)
import Data.Massiv.Vector as V hiding (product)


-- > {-# LANGUAGE TypeApplications #-}
-- > import Data.Massiv.Array
-- > import Data.Fits.Image
-- > import Data.Fits
-- >
-- > decodeExample :: BL.ByteString -> Either String Int
-- > decodeExample bs = do
-- >  hdu <- readPrimaryHDU bs
-- >  arr <- decodeImage @Ix2 $ imageData hdu 
-- >  pure $ arr !> 1 ! 2


-- | Decode an Image into an Image of arbitrary dimensions 'ix'. You will need to inspect the HDU's axes to specify the index type 
--
-- >>> decodeImage @Ix2 @Float (imageData myHdu)
-- Array P Seq (Sz (2 :. 3))
--   [ [ 1.0, 2.0, 3.0 ]
--   , [ 4.0, 5.0, 6.0 ]
--   ]

decodeImage :: MonadThrow m => (Index ix, AxesIndex ix, Prim a, GetPix a) => ImageData -> m (Array P ix a)
decodeImage img = do
  -- either (Left . show) pure $ do
  decodeArray img._imgDimensions._bitpix img._imgDimensions._axes img._imgData


-- | Decode data into an Array of arbitrary dimensions 'ix' specifying 'BitPixFormat' and 'Axes'
--
-- >>> decodeArray @Ix2 @Float ThirtyTwoBitFloat [3, 2] input
-- Array P Seq (Sz (2 :. 3))
--   [ [ 1.0, 2.0, 3.0 ]
--   , [ 4.0, 5.0, 6.0 ]
--   ]
--
decodeArray :: forall ix a m. (AxesIndex ix, Prim a, GetPix a, Index ix, MonadThrow m) => BitPixFormat -> Axes -> BL.ByteString -> m (Array P ix a)
decodeArray f as inp = do
  v <- runGetThrow getAll inp
  fromVector as v
  where
    getAll :: Get (Vector DS a)
    getAll = getAxesVector (getPix f) as

    fromVector :: forall ix a m. (AxesIndex ix, Index ix, Prim a, MonadThrow m) => [Axis] ->Array DS Ix1 a -> m (Array P ix a)
    fromVector as v = do
      ix <- axesIndex $ rowMajorAxes as
      let vc = compute v
      resizeM (Sz ix) vc



-- | Decode Axes as a delayed 1d vector
getAxesVector :: Get a -> Axes -> Get (Vector DS a)
getAxesVector get as = do
  sreplicateM (Sz1 (totalPix as)) get


runGetThrow :: forall a m. MonadThrow m => Get a -> BL.ByteString -> m a
runGetThrow get inp =
  case runGetOrFail get inp of
    Left (_, bytes, e) -> throwM $ ParseError bytes e
    Right (_, _, a) -> pure a


-- | Encode an Array as an Image
--
-- >>> encodeImage array
-- ImageData:
--   data = 78 Bytes
--   dimensions =
--     format = 32 bit IEEE single precision float
--     axes = [3,2]

encodeImage :: forall ix a. (PutArray ix, Index ix, AxesIndex ix, PutPix a, Prim a) => Array P ix a -> ImageData
encodeImage arr =
  let ax = sizeAxes $ size arr
      dim = Dimensions { _axes = ax, _bitpix = bitPixFormat @a Proxy }
      dat = encodeArray arr
  in ImageData { _imgDimensions = dim, _imgData = dat }


-- | Encode an Array as a Lazy ByteString based on the type of the element 'a'
--
-- >>> myArray = decodeArray @Ix2 @Float ThirtyTwoBitFloat [3, 2] input
-- >>> output = encodeArray myArray
encodeArray :: (PutArray ix, Index ix, PutPix a, Prim a) => Array P ix a -> BL.ByteString
encodeArray = runPut . putArray





-- | The total number of pixels to read from the input ByteString
totalPix :: Axes -> Int
totalPix = product


-- | Normal 'Axes' are sorted with the inner-most axis first. RowMajorAxes are the reverse, with the outer dimension first
newtype RowMajorAxes = RowMajorAxes { axes :: [Axis] }
  deriving (Show)

rowMajorAxes :: Axes -> RowMajorAxes
rowMajorAxes as = RowMajorAxes (L.reverse as)


class AxesIndex ix where
  axesIndex :: MonadThrow m => RowMajorAxes -> m ix
  indexAxes :: ix -> RowMajorAxes

instance AxesIndex Ix1 where
  axesIndex (RowMajorAxes [i]) = pure i
  axesIndex as = throwM $ AxesMismatch as
  indexAxes n = RowMajorAxes [n]

instance AxesIndex Ix2 where
  axesIndex (RowMajorAxes [c, r]) = do
    ix1 <- axesIndex $ RowMajorAxes [r]
    pure $ c :. ix1
  axesIndex as = throwM $ AxesMismatch as
  indexAxes (c :. r) = RowMajorAxes [r,c]

instance AxesIndex Ix3 where
  axesIndex = axesIndexN
  indexAxes = indexAxesN

instance AxesIndex Ix4 where
  axesIndex = axesIndexN
  indexAxes = indexAxesN

instance AxesIndex Ix5 where
  axesIndex = axesIndexN
  indexAxes = indexAxesN



axesIndexN :: AxesIndex (Lower (IxN n)) => MonadThrow m => RowMajorAxes -> m (IxN n)
axesIndexN (RowMajorAxes (n:as)) = do
    ixl <- axesIndex (RowMajorAxes as)
    pure $ n :> ixl
axesIndexN as = throwM $ AxesMismatch as

indexAxesN :: AxesIndex (Lower (IxN n)) => IxN n -> RowMajorAxes
indexAxesN (d :> ix) =
  let RowMajorAxes ax = indexAxes ix
  -- outermost axis goes last
  in RowMajorAxes $ ax <> [d]


sizeAxes :: (AxesIndex ix, Index ix) => Sz ix -> Axes
sizeAxes (Sz ix) = (indexAxes ix).axes






data ParseError
  = ParseError !ByteOffset !String
  | AxesMismatch !RowMajorAxes
  deriving (Show, Exception)







class GetPix a where
  getPix :: BitPixFormat -> Get a

instance GetPix Int8 where
  getPix EightBitInt       = get
  getPix f = fail $ "Expected Int8, but format is " <> show f

instance GetPix Int16 where
  getPix SixteenBitInt = get
  getPix f = fail $ "Expected Int16, but format is " <> show f

instance GetPix Int32 where
  getPix ThirtyTwoBitInt = get
  getPix f = fail $ "Expected Int32, but format is " <> show f

instance GetPix Int64 where
  getPix SixtyFourBitInt = get
  getPix f = fail $ "Expected Int64, but format is " <> show f

instance GetPix Int where
  getPix EightBitInt = fromIntegral <$> getPix @Int8 EightBitInt
  getPix SixteenBitInt = fromIntegral <$> getPix @Int16 SixteenBitInt
  getPix ThirtyTwoBitInt = fromIntegral <$> getPix @Int32 ThirtyTwoBitInt
  getPix SixtyFourBitInt = fromIntegral <$> getPix @Int64 SixtyFourBitInt
  getPix f = fail $ "Expected Int, but format is " <> show f

instance GetPix Float where
  getPix ThirtyTwoBitFloat = get
  getPix f = fail $ "Expected Float, but format is " <> show f

instance GetPix Double where
  getPix SixtyFourBitFloat = get
  getPix f = fail $ "Expected Double, but format is " <> show f


-- | How to encode an element type. Note that there is no instance for 'Int', since the size is system dependent. Use Int64 or Int32 instead
class PutPix a where
  bitPixFormat :: Proxy a -> BitPixFormat
  putPix :: a -> Put
  default putPix :: Binary a => a -> Put
  putPix = put

instance PutPix Int8 where
  bitPixFormat _ = EightBitInt
instance PutPix Int16 where
  bitPixFormat _ = SixteenBitInt
instance PutPix Int32 where
  bitPixFormat _ = ThirtyTwoBitInt
instance PutPix Int64 where
  bitPixFormat _ = SixtyFourBitInt
instance PutPix Float where
  bitPixFormat _ = ThirtyTwoBitFloat
instance PutPix Double where
  bitPixFormat _ = SixtyFourBitFloat




class PutArray ix where
  putArray :: (PutPix a, Prim a) => Array P ix a -> Put

instance PutArray Ix1 where
  putArray = sfoldl (\b a -> b <> putPix a) mempty

instance PutArray Ix2 where
  putArray = foldOuterSlice putArray

instance PutArray Ix3 where
  putArray = foldOuterSlice putArray

instance PutArray Ix4 where
  putArray = foldOuterSlice putArray

instance PutArray Ix5 where
  putArray = foldOuterSlice putArray
  


