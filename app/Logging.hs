module Logging (LogLevel(..), setLogLevel, info, debug, log) where

import System.IO.Unsafe (unsafePerformIO)
import Protolude hiding (log)

data LogLevel = Debug | Info
  deriving (Eq, Ord, Show)

defaultLogLevel :: LogLevel
defaultLogLevel = Info

-- Yes, this is a bit gross. But mostly harmless.
globalLogLevel :: MVar LogLevel
{-# NOINLINE globalLogLevel #-}
globalLogLevel = unsafePerformIO $ newMVar defaultLogLevel

setLogLevel :: LogLevel -> IO ()
setLogLevel ll = modifyMVar_ globalLogLevel (const $ pure ll)

info :: Text -> IO ()
info = log Info

debug :: Text -> IO ()
debug = log Debug

log :: LogLevel -> Text -> IO ()
log level t = do
  ll <- readMVar globalLogLevel
  when (level >= ll) $ putStrLn t
