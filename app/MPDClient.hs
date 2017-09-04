{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module MPDClient where

import Control.Arrow ((>>>))
import Control.Concurrent.Async.Lifted (async)
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Exception.Safe (MonadCatch, MonadThrow, catchAny, throw, throwString)
import Control.Monad.Base (MonadBase)
import Control.Monad.Reader (ReaderT, ask, reader)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Network.MPD (MPD)
import qualified Network.MPD as MPD
import Protolude hiding (async)

data IdlingState = NotIdling | Starting | Idling (Async (StM MPD [MPD.Subsystem]))

newtype MPDClient a =
  MPDClient {
    runMPDClient :: ReaderT (TVar IdlingState) MPD a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadBase IO,
            MonadCatch, MonadThrow)

instance MonadBaseControl IO MPDClient where
  type StM MPDClient a = Either MPD.MPDError a
  liftBaseWith f = MPDClient $ liftBaseWith $ \q -> f (q . runMPDClient)
  restoreM = MPDClient . restoreM

getTIdlingState :: MPDClient (TVar IdlingState)
getTIdlingState = MPDClient ask

liftMPD :: MPD a -> MPDClient a
liftMPD = MPDClient . lift

idle :: [MPD.Subsystem] -> MPDClient (Async (Either MPD.MPDError [MPD.Subsystem]))
idle subsystems = getTIdlingState >>= (\tIdleState -> do
  idleAsync <- liftIO . atomically $ readTVar tIdleState >>= \case
    NotIdling -> writeTVar tIdleState Starting >> return Nothing
    Starting  -> retry
    Idling a  -> writeTVar tIdleState Starting >> return (Just a)
  case idleAsync of
    Just a ->  noidle
    Nothing -> return ()
  a <- liftMPD . async $ idle' tIdleState
  wasIdling <- liftIO . atomically $ readTVar tIdleState >>= \case
    Idling _ -> return True
    _        -> return False
  when wasIdling $ throwString "Absurd: idling thread should not be active"
  return a
  )
  where
  idle' :: TVar IdlingState -> MPD [MPD.Subsystem]
  idle' tIdleState = MPD.idle subsystems
    `catchAny` (\err -> do
        wasNotIdling <- liftIO . atomically $
          readTVar tIdleState >>= \case
              NotIdling -> return True
              _         -> writeTVar tIdleState NotIdling >> return False
        if wasNotIdling
          then throwString $
            "Absurd: error thrown from idling thread while state is "
            ++ "'NotIdling': " ++ show err
          else throw err
    )

noidle :: MPDClient ()
noidle = getTIdlingState >>= (\tIdleState -> liftMPD $ do
  idleAsync <- liftIO . atomically $ readTVar tIdleState >>= \case
    NotIdling -> return Nothing
    Starting  -> retry
    Idling a  -> return $ Just a
  case idleAsync of
    Nothing -> return ()
    Just a -> liftIO (cancel a) >> MPD.noidle >> (liftIO . atomically $
        writeTVar tIdleState NotIdling
      )
  )
