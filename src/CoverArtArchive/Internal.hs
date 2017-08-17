{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module CoverArtArchive.Internal
    ( getListing
    , getListingArt
    , getFront
    , getFront250
    , getFront500
    , getBack
    , getBack250
    , getBack500
    , getRelease
    , getReleaseFront
    , getReleaseFront250
    , getReleaseFront500
    ) where

import Data.Proxy (Proxy(..))
import Servant.API (JSON, Capture, Get, (:<|>)(..), (:>))
import Servant.Client (ClientM, client)

import CoverArtArchive.Types
import Music.MusicBrainz.Types (MBID(..))

type API = "release" :> Capture "mbid" MBID :> Get '[JSON] Listing
      :<|> "release" :> Capture "mbid" MBID :> Capture "fileid" FileID :> Get '[JPEGContent] JPEG
      :<|> "release" :> Capture "mbid" MBID :> "front" :> Get '[JPEGContent] JPEG
      :<|> "release" :> Capture "mbid" MBID :> "front-250" :> Get '[JPEGContent] JPEG
      :<|> "release" :> Capture "mbid" MBID :> "front-500" :> Get '[JPEGContent] JPEG
      :<|> "release" :> Capture "mbid" MBID :> "back" :> Get '[JPEGContent] JPEG
      :<|> "release" :> Capture "mbid" MBID :> "back-250" :> Get '[JPEGContent] JPEG
      :<|> "release" :> Capture "mbid" MBID :> "back-500" :> Get '[JPEGContent] JPEG
      :<|> "release-group" :> Capture "mbid" MBID :> Get '[JSON] Listing
      :<|> "release-group" :> Capture "mbid" MBID :> "front" :> Get '[JPEGContent] JPEG
      :<|> "release-group" :> Capture "mbid" MBID :> "front-250" :> Get '[JPEGContent] JPEG
      :<|> "release-group" :> Capture "mbid" MBID :> "front-500" :> Get '[JPEGContent] JPEG

coverArtArchiveAPI :: Proxy API
coverArtArchiveAPI  = Proxy

getListing         :: MBID -> ClientM Listing
getListingArt      :: MBID -> FileID -> ClientM JPEG
getFront           :: MBID -> ClientM JPEG
getFront250        :: MBID -> ClientM JPEG
getFront500        :: MBID -> ClientM JPEG
getBack            :: MBID -> ClientM JPEG
getBack250         :: MBID -> ClientM JPEG
getBack500         :: MBID -> ClientM JPEG
getRelease         :: MBID -> ClientM Listing
getReleaseFront    :: MBID -> ClientM JPEG
getReleaseFront250 :: MBID -> ClientM JPEG
getReleaseFront500 :: MBID -> ClientM JPEG
getListing
  :<|> getListingArt
  :<|> getFront
  :<|> getFront250
  :<|> getFront500
  :<|> getBack
  :<|> getBack250
  :<|> getBack500
  :<|> getRelease
  :<|> getReleaseFront
  :<|> getReleaseFront250
  :<|> getReleaseFront500 = client coverArtArchiveAPI
