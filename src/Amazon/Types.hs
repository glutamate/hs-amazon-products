{-# LANGUAGE OverloadedStrings #-}

module Amazon.Types
    ( timeFormat
    , AccessID
    , AccessSecret
    , AssociateTag

    , AmazonEndpoint (..)
    , AmazonConf (..)
    , AmazonFailure (..)

    , ResponseGroup (..)

    , OperationRequest (..)
    , AmazonError (..)

    , xpOperationRequest
    , xpAmazonError
    ) where

import           Control.Monad.Trans.Error (Error, noMsg, strMsg)
import           Data.ByteString.Lazy      as LBS
import           Data.Map                  as Map
import           Data.Text                 as T
import           Data.XML.Pickle
import           Data.XML.Types
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

data AmazonFailure = AmazonFailure (Maybe AmazonError)
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

data OperationRequest = OperationRequest
        { opHTTPHeaders           :: Map Text Text
        , opRequestId             :: Text
        , opArguments             :: Map Text Text
        , opRequestProcessingTime :: Double
        } deriving (Eq, Show)

xpOperationRequest :: PU [Node] OperationRequest
xpOperationRequest =
    xpWrap wrap unwrap $
    xp4Tuple
        (xpElemNodes "{http://ecs.amazonaws.com/doc/2011-08-01/}HTTPHeaders" $
            xpAll $ xpElemAttrs "{http://ecs.amazonaws.com/doc/2011-08-01/}Header" $
                xp2Tuple (xpAttr "{http://ecs.amazonaws.com/doc/2011-08-01/}Name" xpId)
                         (xpAttr "{http://ecs.amazonaws.com/doc/2011-08-01/}Value" xpId))
        (xpElemText "{http://ecs.amazonaws.com/doc/2011-08-01/}RequestId")
        (xpElemNodes "{http://ecs.amazonaws.com/doc/2011-08-01/}Arguments" $
            xpAll $ xpElemAttrs "{http://ecs.amazonaws.com/doc/2011-08-01/}Argument" $
                xp2Tuple (xpAttr "{http://ecs.amazonaws.com/doc/2011-08-01/}Name" xpId)
                         (xpAttr "{http://ecs.amazonaws.com/doc/2011-08-01/}Value" xpId))
        (xpElemText "{http://ecs.amazonaws.com/doc/2011-08-01/}RequestProcessingTime")
    where wrap :: ([(Text, Text)], Text, [(Text, Text)], Text) -> OperationRequest
          wrap (headers, reqId, args, pTime) =
            OperationRequest (Map.fromList headers) reqId (Map.fromList args)
                (read $ T.unpack pTime)

          unwrap :: OperationRequest -> ([(Text, Text)], Text, [(Text, Text)], Text)
          unwrap (OperationRequest headers reqId args pTime) =
            ((Map.toList headers), reqId, (Map.toList args), (T.pack $ show pTime))

data AmazonError = AmazonError
        { errorCode      :: Text
        , errorMessage   :: Text
        , errorRequestId :: Text
        } deriving (Eq, Show)

xpAmazonError :: PU [Node] AmazonError
xpAmazonError =
    xpWrap wrap unwrap $
    xp2Tuple
        (xpElemNodes "{http://ecs.amazonaws.com/doc/2011-08-01/}Error" $
            xp2Tuple (xpElemText "{http://ecs.amazonaws.com/doc/2011-08-01/}Code")
                     (xpElemText "{http://ecs.amazonaws.com/doc/2011-08-01/}Message"))
        (xpElemText "{http://ecs.amazonaws.com/doc/2011-08-01/}RequestId")
    where wrap :: ((Text, Text), Text) -> AmazonError
          wrap ((code, message), reqId) = AmazonError code message reqId

          unwrap :: AmazonError -> ((Text, Text), Text)
          unwrap (AmazonError code message reqId) = ((code, message), reqId)
