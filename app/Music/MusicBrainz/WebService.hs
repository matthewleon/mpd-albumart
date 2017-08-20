{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Music.MusicBrainz.WebService (searchReleasesByArtistAndRelease) where

import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadThrow, runResourceT, throwM)
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (Consumer, (.|), ($$))
import Data.Conduit.Binary (sourceLbs)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Data.XML.Types (Event)
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (Request, newManager, httpLbs, parseUrlThrow, requestHeaders, tlsManagerSettings, responseBody)
import Network.HTTP.Types.Header (hUserAgent)
import Text.XML.Stream.Parse (XmlException(XmlException), parseBytes, def, tag', tagIgnoreAttrs, requireAttr, attr, force, many, ignoreAnyTreeContent)

import qualified Music.MusicBrainz.Types as MB

musicBrainzWSSearch :: MonadIO m => Text -> Text -> Text -> Maybe Int -> Maybe Int -> m BL.ByteString
musicBrainzWSSearch agent reqtype query mlimit moffset = do
    let url = "https://musicbrainz.org/ws/2/" ++ T.unpack reqtype ++ "/?query=" ++ urlEncode (T.unpack query) ++ limit mlimit ++ offset moffset
    userAgentSimpleHttp agent url
    where
        limit Nothing = ""
        limit (Just l) = "&limit=" ++ show l
        offset Nothing = ""
        offset (Just o) = "&offset=" ++ show o

sinkReleaseList :: MonadThrow m => Consumer Event m [(Int, MB.MBID)]
sinkReleaseList =
  force "metadata required" (tagIgnoreAttrs "{http://musicbrainz.org/ns/mmd-2.0#}metadata" $
    force "release-list required" (tagIgnoreAttrs "{http://musicbrainz.org/ns/mmd-2.0#}release-list" $
      many parseRelease
  ))

forceReadDec :: Integral a => Text -> a
forceReadDec = (\(Right (d, _)) -> d) . TR.decimal

parseRelease :: MonadThrow m => Consumer Event m (Maybe (Int, MB.MBID))
parseRelease = tag' "{http://musicbrainz.org/ns/mmd-2.0#}release" (liftA2 (,) (requireAttr "id") (attr "{http://musicbrainz.org/ns/ext#-2.0}score")) $ \(rid,score) -> do
    _ <- many ignoreAnyTreeContent
    case MB.fromText rid of
      Nothing -> throwM $
        XmlException (T.unpack $ "Cannot parse MBID from: " <> rid) Nothing
      Just mbid -> return (maybe 0 forceReadDec score, mbid)

searchReleasesByArtistAndRelease :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => Text -> Text -> Text -> Maybe Int -> Maybe Int -> m [(Int, MB.MBID)]
searchReleasesByArtistAndRelease agent artist release mlimit moffset = do
    lbs <- musicBrainzWSSearch agent "release" (T.concat ["artist:\"", artist, "\" AND release:\"", release, "\""]) mlimit moffset
    runResourceT $ sourceLbs lbs .| parseBytes def $$ sinkReleaseList

userAgentSimpleHttp :: MonadIO m => Text -> String -> m BL.ByteString
userAgentSimpleHttp userAgent url = liftIO $ do
  man <- newManager tlsManagerSettings
  initReq <- liftIO $ parseUrlThrow url
  let utf8UserAgent = TextEncoding.encodeUtf8 userAgent
      req = initReq {
    requestHeaders = (hUserAgent, utf8UserAgent) : requestHeaders initReq
  }
  responseBody <$> httpLbs (setConnectionClose req) man

  where
  setConnectionClose :: Request -> Request
  setConnectionClose req = req{requestHeaders = ("Connection", "close") : requestHeaders req}
