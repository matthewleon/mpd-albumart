module Music.MPD.AlbumArt.Event.SystemChange (
  SystemChangeEvent(..)
, registerSystemChangeEvent
) where

import Data.Bits (Bits(..))
import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Network.MPD (Subsystem, withMPD, currentSong, idle)
import SDL.Event (RegisteredEventData(registeredEventCode), RegisteredEventType, Timestamp, registerEvent, emptyRegisteredEvent)

newtype SystemChangeEvent = SystemChangeEvent (Set Subsystem)
  deriving (Show)

eventCode :: Bits a => SystemChangeEvent -> a
eventCode (SystemChangeEvent ss) =
  foldl' (\i s -> setBit i $ fromEnum s) zeroBits ss

fromCode :: Bits a => a -> SystemChangeEvent
fromCode bits = SystemChangeEvent $
  foldl' addSystemFromBits Set.empty allSubsystems
  where
  allSubsystems = enumFrom minBound
  addSystemFromBits ss s =
    if testBit bits $ fromEnum s
    then Set.insert s ss
    else ss

toSystemChangeEvent
  :: RegisteredEventData
  -> Timestamp
  -> IO (Maybe SystemChangeEvent)
toSystemChangeEvent d _ = return . Just . fromCode $ registeredEventCode d

fromSystemChangeEvent :: SystemChangeEvent -> RegisteredEventData
fromSystemChangeEvent e = emptyRegisteredEvent {
  registeredEventCode = eventCode e
}

registerSystemChangeEvent :: IO (Maybe (RegisteredEventType SystemChangeEvent))
registerSystemChangeEvent =
  registerEvent toSystemChangeEvent (return . fromSystemChangeEvent)
