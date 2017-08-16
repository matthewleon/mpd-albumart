module Music.MusicBrainz.Types (
  MBID
, fromUUID
, toUUID
, fromText
, toText
, fromString
, toString
) where

import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID

newtype MBID = MBID UUID
  deriving (Show, Eq, Ord)

fromUUID :: UUID -> MBID
fromUUID = MBID

toUUID :: MBID -> UUID
toUUID (MBID mbid) = mbid

fromText :: Text -> Maybe MBID
fromText = fmap MBID . UUID.fromText

toText :: MBID -> Text
toText = UUID.toText . toUUID

fromString :: String -> Maybe MBID
fromString = fmap MBID . UUID.fromString

toString :: MBID -> String
toString = UUID.toString . toUUID
