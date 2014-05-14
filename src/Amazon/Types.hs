module Amazon.Types
    ( timeFormat
    , AccessID
    , AccessSecret
    , AssociateTag

    , AmazonEndpoint (..)
    , AmazonConf (..)
    , AmazonFailure (..)

    , ResponseGroup
    ) where

import           Control.Monad.Trans.Error (Error, noMsg, strMsg)
import           Data.ByteString.Lazy      as LBS
import           Data.Text                 as T
import           Data.XML.Pickle
import           Network.HTTP.Conduit

timeFormat :: String
timeFormat = "%Y-%m-%dT%H:%M:%S%QZ"

type AccessID     = Text
type AccessSecret = LBS.ByteString
type AssociateTag = Text

data AmazonEndpoint = AmazonEndpoint
        { endpointURL  :: String
        , endpointHost :: Text
        , endpointPath :: Text
        } deriving (Eq, Show)

data AmazonConf = AmazonConf
        { amazonEndpoint     :: AmazonEndpoint
        , amazonManager      :: Manager
        , amazonAccessId     :: AccessID
        , amazonAccessSecret :: AccessSecret
        , amazonAssociateTag :: AssociateTag
        }

data AmazonFailure = AmazonFailure (Maybe Text)
                   | ParseFailure  (Maybe UnpickleError)
                   | OtherFailure  (Maybe Text)
                   deriving (Show)

instance Error AmazonFailure where
    noMsg  = OtherFailure Nothing
    strMsg = OtherFailure . Just . T.pack

data ResponseGroup = Accessories
                   | AlternateVersions
                   | BrowseNodeInfo
                   | BrowseNodes
                   | Cart
                   | CartNewReleases
                   | CartTopSellers
                   | CartSimilarities
                   | EditoralReview
                   | Images
                   | ItemAttributes
                   | ItemIds
                   | Large
                   | Medium
                   | MostGifted
                   | MostWishedFor
                   | NewReleases
                   | OfferFull
                   | OfferListings
                   | Offers
                   | OfferSummary
                   | PromotionSummary
                   | RelatedItems
                   | Request
                   | Reviews
                   | SalesRank
                   | SearchBins
                   | Similarities
                   | Small
                   | TopSellers
                   | Tracks
                   | Variations
                   | VariationImages
                   | VariationMatrix
                   | VariationOffers
                   | VariationSummary
                   deriving (Eq, Show)
