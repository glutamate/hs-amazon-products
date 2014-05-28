{-# LANGUAGE OverloadedStrings #-}

module Amazon.Types
    ( nsName
    , timeFormat
    , AccessID
    , AccessSecret
    , AssociateTag

    , AmazonEndpoint (..)
    , AmazonConf (..)
    , AmazonFailure (..)

    , ResponseGroup (..)

    , OperationRequest (..)
    , AmazonError (..)
    , RequestContainer (..)

    , xpResponseGroup
    , xpOperationRequest
    , xpAmazonError
    , xpRequestContainer

    , Parameterize (..)
    ) where

import           Control.Monad.Trans.Error (Error, noMsg, strMsg)
import           Data.ByteString.Lazy      as LBS
import           Data.Map                  as Map
import           Data.Text                 as T
import           Data.XML.Pickle
import           Data.XML.Types
import           Network.HTTP.Conduit

nsName :: Text -> Name
nsName n = Name n (Just "http://webservices.amazon.com/AWSECommerceService/2011-08-01")
            Nothing

timeFormat :: String
timeFormat = "%Y-%m-%dT%H:%M:%S.000Z"

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
                   deriving (Eq, Show, Read)

xpResponseGroup :: PU Text ResponseGroup
xpResponseGroup = xpPrim

data OperationRequest = OperationRequest
        { opRequestId             :: Text
        , opArguments             :: Map Text Text
        , opRequestProcessingTime :: Double
        } deriving (Eq, Show)

xpOperationRequest :: PU [Node] OperationRequest
xpOperationRequest =
    xpWrap (\(r, a, t) -> OperationRequest r (Map.fromList a) t)
           (\(OperationRequest r a t) -> (r, (Map.toList a), t)) $
    xp3Tuple
        (xpElemText (nsName "RequestId"))
        (xpElemNodes (nsName "Arguments") $
            xpList $ xpElemAttrs (nsName "Argument") $
                xp2Tuple (xpAttr "Name" xpId)
                         (xpAttr "Value" xpId))
        (xpElemNodes (nsName "RequestProcessingTime") $ xpContent xpPrim)

data AmazonError = AmazonError
        { errorCode      :: Text
        , errorMessage   :: Text
        , errorRequestId :: Text
        } deriving (Eq, Show)

xpAmazonError :: PU [Node] AmazonError
xpAmazonError =
    xpWrap wrap unwrap $
    xp2Tuple
        (xpElemNodes (nsName "Error") $
            xp2Tuple (xpElemText (nsName "Code"))
                     (xpElemText (nsName "Message")))
        (xpElemText (nsName "RequestId"))
    where wrap :: ((Text, Text), Text) -> AmazonError
          wrap ((code, message), reqId) = AmazonError code message reqId

          unwrap :: AmazonError -> ((Text, Text), Text)
          unwrap (AmazonError code message reqId) = ((code, message), reqId)

class Parameterize a where
    toParams :: a -> [(Text, Text)]

----

data RequestContainer a = RequestContainer
        { requestIsValid :: Bool
        , requestSubset  :: a
        } deriving (Eq, Show)

xpRequestContainer :: (Eq a, Show a) => Name -> PU [Node] a -> PU [Node] (RequestContainer a)
xpRequestContainer elemName xpSubset =
    xpWrap (\(a, b) -> RequestContainer a b)
           (\(RequestContainer a b) -> (a, b)) $
    xp2Tuple
        (xpElemNodes (nsName "IsValid") $
            xpContent xpPrim)
        (xpElemNodes elemName $ xpSubset)
