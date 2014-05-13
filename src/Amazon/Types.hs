module Amazon.Types
    ( AccessID
    , AccessSecret
    , AssociateTag

    , AmazonConf (..)
    , AmazonFailure (..)
    ) where

import           Control.Monad.Trans.Error (Error, noMsg, strMsg)
import           Data.Text                 as T
import           Data.XML.Pickle
import           Network.HTTP.Conduit

timeFormat :: String
timeFormat = "%Y-%m-%dT%H:%M:%S%QZ"

type AccessID     = Text
type AccessSecret = Text
type AssociateTag = Text

data AmazonConf = AmazonConf
        { amazonEndpoint     :: Text
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
