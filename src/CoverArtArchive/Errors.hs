module CoverArtArchive.Errors
    ( QueryError(..)
    , ListingError(..)
    , ImageError(..)
    , mkListingQueryError
    , mkImageQueryError
    ) where

import Servant.Client (ServantError(..))
import Network.HTTP.Types (statusCode)

data ListingError = ListingInvalidMbid
                  | ListingNoRelease
                  | ListingInvalidMethod
                  | ListingNoResponseForAcceptHeader
                  | ListingRateLimitExceeded

statusCodeToListingError :: Int -> Maybe ListingError
statusCodeToListingError 400 = Just ListingInvalidMbid
statusCodeToListingError 404 = Just ListingNoRelease
statusCodeToListingError 405 = Just ListingInvalidMethod
statusCodeToListingError 406 = Just ListingNoResponseForAcceptHeader
statusCodeToListingError 503 = Just ListingRateLimitExceeded
statusCodeToListingError _   = Nothing

data ImageError = ImageInvalidMbid
                | ImageNoReleaseOrNoImage
                | ImageInvalidMethod
                | ImageRateLimitExceeded

statusCodeToImageError :: Int -> Maybe ImageError
statusCodeToImageError 400 = Just ImageInvalidMbid
statusCodeToImageError 404 = Just ImageNoReleaseOrNoImage
statusCodeToImageError 405 = Just ImageInvalidMethod
statusCodeToImageError 503 = Just ImageRateLimitExceeded
statusCodeToImageError _   = Nothing

data QueryError a = QueryError (Maybe a) ServantError

mkListingQueryError :: ServantError -> QueryError ListingError
mkListingQueryError err@(FailureResponse _ status _ _)
  | code <- statusCode status
  = QueryError (statusCodeToListingError code) err
mkListingQueryError err = QueryError Nothing err

mkImageQueryError :: ServantError -> QueryError ImageError
mkImageQueryError err@(FailureResponse _ status _ _)
  | code <- statusCode status
  = QueryError (statusCodeToImageError code) err
mkImageQueryError err = QueryError Nothing err
