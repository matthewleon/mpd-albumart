{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Exception.Safe (throw, throwString, catchAny)
import Music.MusicBrainz (searchSong)
import Network.MPD (MPD, Song(sgTags), Subsystem(..), withMPD, currentSong, idle, playlistInfo)
import SDL hiding (Debug)
import SDL.Hint (HintPriority(OverridePriority), Hint(..), RenderScaleQuality(..), setHintWithPriority)
import SDL.Image (loadTexture, decodeTexture)
import qualified Data.Text as T
import Foreign.C (CInt)

import qualified CoverArtArchive as CAA
import CoverArtArchive.Types (JPEG(JPEG))
import Music.MPD.AlbumArt.Event.SystemChange (SystemChangeEvent)
import qualified Music.MPD.AlbumArt.Event.SystemChange as SystemChange

import Logging (LogLevel(Debug), setLogLevel, info, debug)
import Prelude (String)
import Protolude hiding (get)

title :: Text
-- TODO: change
title  = "Album cover"

windowConfig :: WindowConfig
windowConfig = WindowConfig
  { windowBorder       = True
  , windowHighDPI      = True
  , windowInputGrabbed = False
  , windowMode         = Windowed
  , windowOpenGL       = Nothing
  , windowPosition     = Wherever
  , windowResizable    = True
  , windowInitialSize  = V2 800 600
  , windowVisible      = True
  }

resizeWatch :: Renderer -> Texture -> EventWatchCallback
resizeWatch renderer texture ev =
  case eventPayload ev of
    WindowSizeChangedEvent _ -> draw renderer texture
    _ -> return ()

main :: IO ()
main = do
  setLogLevel Debug
  info "starting"
  getCurrentSongCover Nothing >>= initSystems

initSystems :: Maybe (Song, JPEG) -> IO ()
initSystems maybeInitSongCover =
  initializeAll >> SystemChange.register >>= \case
    Nothing -> throwString "Unable to register user event with SDL."
    Just (RegisteredEventType pushSystemChangeEvent getSystemChangeEvent) -> do
      void . forkIO $ mpdThread pushSystemChangeEvent
      -- when stretching album covers, use bilinear filtering
      -- default is nearest neighbor
      setHintWithPriority OverridePriority HintRenderScaleQuality ScaleLinear
      window <- createWindow title windowConfig
      renderer <- createRenderer window (-1) defaultRenderer
      watch <- update renderer Nothing (snd <$> maybeInitSongCover)
      appLoop renderer watch getSystemChangeEvent maybeInitSongCover

mpdThread :: (SystemChangeEvent -> IO EventPushResult) -> IO ()
mpdThread push = go
  where
  go = SystemChange.fromList <$> withMPD' (idle [PlayerS, PlaylistS])
    >>= push >>= \case
      EventPushSuccess -> go
      EventPushFiltered -> throwString "System change event filtered."
      EventPushFailure s -> throwString $ T.unpack s

appLoop :: Renderer -> Maybe EventWatch -> (Event -> IO (Maybe SystemChangeEvent)) -> Maybe (Song, JPEG) -> IO ()
appLoop renderer watch getSystemChangeEvent initSongCover =
  waitEvent >>= go watch initSongCover
  where
  go :: Maybe EventWatch -> Maybe (Song, JPEG) -> Event -> IO ()
  go watch previousSongCover ev = case eventPayload ev of
     KeyboardEvent keyboardEvent
       |  keyboardEventKeyMotion keyboardEvent == Pressed &&
          keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
       -> return ()
     _ ->
      getSystemChangeEvent ev >>= \case
        Just systemChangeEvent -> do
          songCover <- getCurrentSongCover previousSongCover
          watch' <- update renderer watch $ snd <$> songCover
          waitEvent >>= go watch' songCover
        Nothing -> waitEvent >>= go watch previousSongCover

update :: Renderer -> Maybe EventWatch -> Maybe JPEG -> IO (Maybe EventWatch)
update renderer maybeWatch maybeCover = do
  maybe (pure ()) delEventWatch maybeWatch
  case maybeCover of
    Just (JPEG cover) -> do
      texture <- decodeTexture renderer cover
      draw renderer texture
      Just <$> addEventWatch (resizeWatch renderer texture)
    Nothing -> pure maybeWatch

getCurrentSongCover :: Maybe (Song, JPEG) -> IO (Maybe (Song, JPEG))
getCurrentSongCover maybePrevious = do
  currentSong <- getCurrentSong
  case maybePrevious of
    Just previous@(previousSong, _)
      |  previousSong == currentSong
      -> pure $ Just previous
    _ -> Just . (currentSong,) <$> getCover currentSong
  `catchAny` \e -> do
    info $ "unable to obtain album art: " <> T.pack (displayException e)
    pure Nothing

newtype SongFetchException = SongFetchException Text
  deriving (Show, Typeable)
instance Exception SongFetchException where
  displayException (SongFetchException t) = T.unpack t

getCurrentSong :: IO Song
getCurrentSong = do
  debug "querying mpd for current song"
  song <- throwFetchOnNothing "no current song" =<< withMPD' currentSong
  debug $ "current song: " <> show song
  pure song

getCover :: Song -> IO JPEG
getCover song = do
  debug "fetching mbid"
  mbid <- throwFetchOnNothing "unable to find mbid for song" =<< searchSong song
  debug $ "mbid: " <> show mbid
  debug "fetching art"
  jpeg <- throwFetchOnLeft "unable to get album art for mbid" =<< CAA.getFront mbid
  debug "fetched art"
  pure jpeg
  where
  throwFetchOnLeft t = either (const $ throwFetch t) return

draw :: Renderer -> Texture -> IO ()
draw renderer texture = do
  textureInfo <- queryTexture texture
  get (rendererViewport renderer) >>= \case
    Just (Rectangle _ viewSize) -> do
      let texSize = V2 (textureWidth textureInfo) (textureHeight textureInfo)
          stretchedSize = scaleProportionally texSize viewSize
          texOrigin = centerOrigin stretchedSize viewSize
          dstRect = Rectangle texOrigin stretchedSize
      copy renderer texture Nothing $ Just dstRect
      present renderer
    Nothing -> return ()

scaleProportionally :: V2 CInt -> V2 CInt -> V2 CInt
scaleProportionally (V2 sourcew sourceh) (V2 destw desth) =
  let ratioWidth  = fromIntegral destw / fromIntegral sourcew
      ratioHeight = fromIntegral desth / fromIntegral sourceh
      ratio      :: Double
      ratio       = min ratioWidth ratioHeight
      newWidth    = round $ ratio * fromIntegral sourcew
      newHeight   = round $ ratio * fromIntegral sourceh
  in  V2 newWidth newHeight

centerOrigin :: V2 CInt -> V2 CInt -> Point V2 CInt
centerOrigin (V2 smallerw smallerh) (V2 largerw largerh) =
  let newX = (largerw - smallerw) `div` 2
      newY = (largerh - smallerh) `div` 2
  in  P $ V2 newX newY

withMPD' :: MPD a -> IO a
withMPD' m = withMPD m >>= either throw return

throwOnNothing :: Exception e => e -> Maybe a -> IO a
throwOnNothing e = maybe (throw e) return

throwFetchOnNothing :: Text -> Maybe a -> IO a
throwFetchOnNothing t = throwOnNothing (SongFetchException t)

throwFetch :: Text -> IO a
throwFetch = throw . SongFetchException
