{-# LANGUAGE OverloadedStrings #-}

module Music.MusicBrainz (search, searchSong) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (throwString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.MPD (Song(sgTags), Metadata(..), toText)
import Music.MusicBrainz.Types (MBID)
import Music.MusicBrainz.WebService (searchReleasesByArtistAndRelease)
import Safe (headMay)

search :: MonadIO m => Text -> Text -> m [(Int, MBID)]
search artist release = liftIO $
  searchReleasesByArtistAndRelease userAgent artist release Nothing Nothing
  where userAgent = "mpd-albumart/dev (ml@matthewleon.com)"

-- TODO: refine errors
searchSong :: MonadIO m => Song -> m [(Int, MBID)]
searchSong song = fromMaybe (return []) $
  search <$> (maybeAlbumArtist <|> maybeArtist) <*> maybeAlbum
  where
    tags = sgTags song
    maybeAlbumArtist = getTextTag AlbumArtist
    maybeArtist = getTextTag Artist
    maybeAlbum = getTextTag Album

    getTextTag :: Metadata -> Maybe Text
    getTextTag tag = toText <$> (headMay =<< Map.lookup tag tags)

getSongInfo :: Song -> IO ()
getSongInfo song = print tags
  where tags = sgTags song
