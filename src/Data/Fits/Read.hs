{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Data.Fits.Read where

import Control.Exception ( displayException, Exception )
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Bifunctor ( first )
import Data.ByteString.Lazy ( ByteString )
import Data.Maybe ( listToMaybe )
import Data.Text ( Text, unpack )
import qualified Data.ByteString as BS
import qualified Data.Map.Lazy as Map
import qualified Text.Megaparsec as M
import Data.List ( find )

---- local imports
import Data.Fits as Fits
import Data.Fits.MegaParser (ParseErr(..), parseHDU, parseHDUs)
import Data.Fits (HeaderDataUnit(..))


-- | Parse and read all HDUs in the input string
readHDUs :: MonadThrow m => ByteString -> m [HeaderDataUnit]
readHDUs bs = do
    either (throwM . ParseError) pure $ M.runParser parseHDUs "FITS" bs

-- | Parse and read only the Primary HDU from the input string
readPrimaryHDU :: MonadThrow m => ByteString -> m HeaderDataUnit
readPrimaryHDU bs = do
    either (throwM . ParseError) pure $ M.runParser parseHDU "FITS" bs

-- | Look up a keyword in a HeaderDataUnit and parse it into the expected format
getKeyword :: MonadThrow m => Keyword -> (Value -> Maybe a) -> HeaderDataUnit -> m a
getKeyword key fromVal hdu = do
    v <- getKeyword' key hdu._header
    maybeError (InvalidKey key v) $ fromVal v

-- | Look up a keyword in a Header
getKeyword' :: MonadThrow m => Keyword -> Header -> m Value
getKeyword' key h = do
    maybeError (MissingKey key) $ Map.lookup key h._keywords
  where
    findKey :: Keyword -> Header -> Maybe Value
    findKey key h = Map.lookup key h._keywords

-- | Get the HDU at an index and fail with a readable error
getHDU :: MonadThrow m => String -> Int -> [HeaderDataUnit] -> m HeaderDataUnit
getHDU name n hdus = do
    maybeError (MissingHDU name n) $ listToMaybe $ drop n hdus

maybeError :: MonadThrow m => FitsError -> Maybe a -> m a
maybeError e Nothing = throwM e
maybeError _ (Just a) = pure a

data FitsError
    = ParseError ParseErr
    | MissingKey Keyword
    | InvalidKey Keyword Value
    | MissingHDU String Int 
    | InvalidData String
    deriving (Eq, Exception)

instance Show FitsError where
    show (ParseError e) = displayException e
    show (MissingKey (Keyword k)) = "Keyword Missing: " <> unpack k
    show (InvalidKey (Keyword k) val) = "Keyword: " <> unpack k <> " was invalid. Got " <> show val
    show (MissingHDU name n) = "HDU Missing: " <> name <> " at index " <> show n
    show (InvalidData err) = "Data Invalid: " <> err

-- -- | An example of how to use the library
-- example :: IO ()
-- example = do
--     bs <- BS.readFile  "./fits_files/nso_dkist.fits"
--
--     (tel, obs, dm) <- throwLeft $ exampleReadMyData bs
--
--     putStrLn $ "TELESCOPE: " <> unpack tel
--     putStrLn $ "OBSERVATORY: " <> unpack obs
--     putStrLn $ "DATAMIN: " <> show dm
--
--   where
--     throwLeft :: Show e => Either e a -> IO a
--     throwLeft (Left e) = fail $ show e
--     throwLeft (Right a) = return a
--
--     -- You can parse the file and lookup relevant data in the same function
--     exampleReadMyData :: MonadThrow m => ByteString -> m (Text, Text, Float)
--     exampleReadMyData bs = do
--       hdus <- readHDUs bs
--       hdu <- getHDU "Main Binary Table" 1 hdus
--       tel <- getKeyword "TELESCOP" toText hdu
--       obs <- getKeyword "OBSRVTRY" toText hdu
--       dm <- getKeyword "DATAMIN" toFloat hdu
--       return (tel, obs, dm)
--
