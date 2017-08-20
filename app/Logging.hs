module Logging (LogLevel, setLogLevel, info, debug, log) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Protolude hiding (log)

data LogLevel = Debug | Info
  deriving (Eq, Ord, Show)

defaultLogLevel :: LogLevel
defaultLogLevel = Info

-- Yes, this is a bit gross. But mostly harmless.
globalLogLevel :: IO (IORef LogLevel)
globalLogLevel = newIORef defaultLogLevel

setLogLevel :: LogLevel -> IO ()
setLogLevel ll = do
  globalLogLevelRef <- globalLogLevel
  writeIORef globalLogLevelRef ll

info :: Text -> IO ()
info = log Info

debug :: Text -> IO ()
debug = log Debug

log :: LogLevel -> Text -> IO ()
log level t = do
  ll <- readIORef =<< globalLogLevel
  when (level >= ll) $ putStrLn t
