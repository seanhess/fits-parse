{-# LANGUAGE OverloadedStrings #-}
module Example where

import Control.Monad.Catch (MonadThrow)
import Data.Bifunctor (first)
import Data.Fits
import Data.Fits.Read
import Data.Text (Text, unpack)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy (ByteString)

-- To be copied into documentation
example :: IO ()
example = do
    bs <- LBS.readFile  "./fits_files/nso_dkist.fits"

    (tel, obs, dm) <- exampleIO bs

    putStrLn $ "TELESCOPE: " <> unpack tel
    putStrLn $ "OBSERVATORY: " <> unpack obs
    putStrLn $ "DATAMIN: " <> show dm

  where
    -- You can parse the file and lookup relevant data in 'IO' or 'Either String'
    exampleReadMyData :: MonadThrow m => ByteString -> m (Text, Text, Float)
    exampleReadMyData bs = do
      hdus <- readHDUs bs
      hdu <- getHDU "Main Binary Table" 1 hdus
      tel <- getKeyword "TELESCOP" toText hdu
      obs <- getKeyword "OBSRVTRY" toText hdu
      dm <- getKeyword "DATAMIN" toFloat hdu
      return (tel, obs, dm)

    exampleEither :: ByteString -> Either String (Text, Text, Float)
    exampleEither bs = first show $ exampleReadMyData bs

    exampleIO :: ByteString -> IO (Text, Text, Float)
    exampleIO = exampleReadMyData
