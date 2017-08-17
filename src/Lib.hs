{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Lib
    ( someFunc
    ) where

import Control.Concurrent (forkIO)
import Control.Exception.Safe (throw, throwString)
import Control.Monad (void, (<=<))
import Control.Monad.IO.Class (MonadIO)
import Data.Either (either)
import Data.Maybe (maybe)
import Data.Monoid ((<>))
import Music.MusicBrainz (searchSong)
import Network.MPD (MPD, Song(sgTags), Subsystem(..), withMPD, currentSong, idle, playlistInfo)
import SDL
import SDL.Hint (HintPriority(OverridePriority), Hint(..), RenderScaleQuality(..), setHintWithPriority)
import SDL.Image (load)
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C (CInt)

import qualified CoverArtArchive as CAA
import Music.MPD.AlbumArt.Event.SystemChange (SystemChangeEvent)
import qualified Music.MPD.AlbumArt.Event.SystemChange as SystemChange

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
  }

resizeWatch :: Renderer -> Texture -> EventWatchCallback
resizeWatch renderer texture ev =
  case eventPayload ev of
    WindowSizeChangedEvent _ -> draw renderer texture
    _ -> return ()

someFunc :: IO ()
someFunc = initializeAll >> SystemChange.register >>= \case
    Nothing -> throwString "Unable to register user event with SDL."
    Just (RegisteredEventType pushSystemChangeEvent getSystemChangeEvent) -> do
      void . forkIO $ mpdThread pushSystemChangeEvent
      -- when stretching album covers, use bilinear filtering
      -- default is nearest neighbor
      setHintWithPriority OverridePriority HintRenderScaleQuality ScaleLinear
      surface <- load "hosono.jpg"
      window <- createWindow title windowConfig
      renderer <- createRenderer window (-1) defaultRenderer
      texture <- createTextureFromSurface renderer surface
      addEventWatch $ resizeWatch renderer texture
      freeSurface surface
      draw renderer texture
      appLoop getSystemChangeEvent

mpdThread :: (SystemChangeEvent -> IO EventPushResult) -> IO ()
mpdThread push = go
  where
  go = SystemChange.fromList <$> withMPD' (idle [PlayerS, PlaylistS])
    >>= push >>= \case
      EventPushSuccess -> go
      EventPushFiltered -> throwString "System change event filtered."
      EventPushFailure s -> throwString $ T.unpack s

appLoop :: (Event -> IO (Maybe SystemChangeEvent)) -> IO ()
appLoop getSystemChangeEvent = waitEvent >>= go
  where
  go :: Event -> IO ()
  go ev = case eventPayload ev of
     KeyboardEvent keyboardEvent
       |  keyboardEventKeyMotion keyboardEvent == Pressed &&
          keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
       -> return ()
     _ -> do
      getSystemChangeEvent ev >>= \case
        Just systemChangeEvent -> update
        Nothing -> return ()
      waitEvent >>= go

-- TODO: monadify
update :: IO ()
update =
  --print =<< withMPD' (playlistInfo Nothing)
  withMPD' currentSong >>= \case
    Nothing -> putStrLn "no current song"
    Just song -> searchSong song >>= \case
      Nothing -> putStrLn $ "unable to find mbid for song: " <> show song
      Just mbid -> CAA.getFront mbid >>= \case
        Left _ -> putStrLn $ "unable to find album art for mbid: " <> show mbid
        Right jpeg -> print jpeg

draw :: Renderer -> Texture -> IO ()
draw renderer texture = do
  textureInfo <- queryTexture texture
  viewPort <- get $ rendererViewport renderer
  print viewPort
  case viewPort of
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
