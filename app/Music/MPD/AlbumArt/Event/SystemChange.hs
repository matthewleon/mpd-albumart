module Music.MPD.AlbumArt.Event.SystemChange (
  SystemChangeEvent
, fromSet
, fromList
, toSet
, toList
, register
) where

import Data.Bits (Bits(..))
import Data.Foldable (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Network.MPD (Subsystem, withMPD, currentSong, idle)
import SDL.Event (RegisteredEventData(registeredEventCode), RegisteredEventType, Timestamp, registerEvent, emptyRegisteredEvent)

newtype SystemChangeEvent = SystemChangeEvent (Set Subsystem)
  deriving (Show)

fromSet :: Set Subsystem -> SystemChangeEvent
fromSet = SystemChangeEvent

fromList :: [Subsystem] -> SystemChangeEvent
fromList = fromSet . Set.fromList

toSet :: SystemChangeEvent -> Set Subsystem
toSet (SystemChangeEvent ss) = ss

toList :: SystemChangeEvent -> [Subsystem]
toList = Set.toList . toSet

register :: IO (Maybe (RegisteredEventType SystemChangeEvent))
register =
  registerEvent toSystemChangeEvent (return . fromSystemChangeEvent)
  where

  toSystemChangeEvent
    :: RegisteredEventData
    -> Timestamp
    -> IO (Maybe SystemChangeEvent)
  toSystemChangeEvent d _ = return . Just . fromCode $ registeredEventCode d
    where
    fromCode :: Bits a => a -> SystemChangeEvent
    fromCode bits = SystemChangeEvent $
      foldl' addSystemFromBits Set.empty allSubsystems
      where
      allSubsystems = enumFrom minBound
      addSystemFromBits ss s =
        if testBit bits $ fromEnum s
        then Set.insert s ss
        else ss

  fromSystemChangeEvent :: SystemChangeEvent -> RegisteredEventData
  fromSystemChangeEvent e = emptyRegisteredEvent {
    registeredEventCode = eventCode e
  }
    where
    eventCode :: Bits a => SystemChangeEvent -> a
    eventCode (SystemChangeEvent ss) =
      foldl' (\i s -> setBit i $ fromEnum s) zeroBits ss
