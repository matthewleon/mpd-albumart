{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module CoverArtArchive.Types
    ( Listing
    , Image
    , ThumbnailListing
    , ImageType
    , EditID
    , FileID
    , APIURI
    , JPEG(JPEG)
    , JPEGContent
    ) where

import Data.Aeson.Types (FromJSON, parseJSON, typeMismatch)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Text.Read as T.Read
import Network.HTTP.Media.MediaType ((//))
import GHC.Generics
import Network.URI (URI, parseAbsoluteURI)
import Servant.API (ToHttpApiData, Accept(..), MimeUnrender(..))
import Protolude

data Listing = Listing
  { release :: APIURI
  , images  :: [Image]
  } deriving (Show, Generic)

instance FromJSON Listing

data Image = Image
  { types    :: [ImageType]
  , front    :: Bool
  , back     :: Bool
  , edit     :: EditID
  , image    :: APIURI
  , comment  :: Text
  , approved :: Bool
  , id       :: FileID
  , thumbnails :: ThumbnailListing
  } deriving (Show, Generic)
instance FromJSON Image

data ThumbnailListing = ThumbnailListing
  { large :: APIURI
  , small :: APIURI
  } deriving (Show, Generic)
instance FromJSON ThumbnailListing

data ImageType =
    Front
  | Back
  | Booklet
  | Medium
  | Tray
  | Obi
  | Spine
  | Track
  | Liner
  | Sticker 
  | Poster
  | Watermark
  | Other
  deriving (Show, Generic)
instance FromJSON ImageType

newtype EditID = EditID Integer
  deriving (Show, Generic, FromJSON)

newtype FileID = FileID Integer
  deriving (Show, ToHttpApiData)
instance FromJSON FileID where
  parseJSON val = mkParser =<< T.Read.decimal <$> parseJSON val
    where
    mkParser (Right (fid, "")) = pure $ FileID fid
    mkParser _                 = typeMismatch "fileID" val

newtype APIURI = APIURI URI
  deriving (Show)
instance FromJSON APIURI where
  parseJSON val = mkParser =<< parseAbsoluteURI <$> parseJSON val
    where
    mkParser = maybe (typeMismatch "URI" val) (pure . APIURI)

data JPEGContent = JPEGContent
  deriving (Show)
instance Accept JPEGContent where
  contentType _ = "image" // "jpeg"
instance MimeUnrender JPEGContent JPEG where
  mimeUnrender _ = Right . JPEG . BL.toStrict
instance MimeUnrender JPEGContent BL.ByteString where
  mimeUnrender _ = Right
instance MimeUnrender JPEGContent BS.ByteString where
  mimeUnrender _ = Right . BL.toStrict

newtype JPEG = JPEG BS.ByteString
  deriving (Show)
