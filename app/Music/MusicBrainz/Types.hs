{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Music.MusicBrainz.Types (
  MBID
, fromUUID
, toUUID
, fromText
, toText
) where

import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import Servant.API (ToHttpApiData)
import Protolude

newtype MBID = MBID UUID
  deriving (Show, Eq, Ord, ToHttpApiData)

fromUUID :: UUID -> MBID
fromUUID = MBID

toUUID :: MBID -> UUID
toUUID (MBID mbid) = mbid

fromText :: Text -> Maybe MBID
fromText = fmap MBID . UUID.fromText

toText :: MBID -> Text
toText = UUID.toText . toUUID
