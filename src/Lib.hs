{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Lib
    ( someFunc
    ) where

import Network.MPD (withMPD, currentSong, rescan)
import SDL
import SDL.Hint (HintPriority(OverridePriority), Hint(..), RenderScaleQuality(..), setHintWithPriority)
import SDL.Image (load)
import Data.Text (Text)
import Foreign.C (CInt)

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
resizeWatch renderer texture = \ev ->
  case eventPayload ev of
    WindowSizeChangedEvent _ -> draw renderer texture
    _ -> return ()

someFunc :: IO ()
someFunc = do
  _ <- withMPD $ rescan Nothing
  mpdResp <- withMPD currentSong
  putStrLn $ case mpdResp of
    Left err -> show err
    Right song -> show song
  initializeAll
  -- when stretching album covers, use bilinear filtering
  -- default is nearest neighbor
  setHintWithPriority OverridePriority HintRenderScaleQuality ScaleLinear
  surface <- load "hosono.jpg"
  window <- createWindow title windowConfig
  renderer <- createRenderer window (-1) defaultRenderer
  texture <- createTextureFromSurface renderer surface
  freeSurface surface
  addEventWatch $ resizeWatch renderer texture
  draw renderer texture
  appLoop

appLoop :: IO ()
appLoop = waitEvent >>= go
  where
  go :: Event -> IO ()
  go ev =
    case eventPayload ev of
       KeyboardEvent keyboardEvent
         |  keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
         -> return ()
       _ -> waitEvent >>= go
    --rendererDrawColor renderer $= V4 0 0 255 255

draw :: Renderer -> Texture -> IO ()
draw renderer texture = do
  textureInfo <- queryTexture texture
  viewPort <- get $ rendererViewport renderer
  putStrLn $ show viewPort
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
