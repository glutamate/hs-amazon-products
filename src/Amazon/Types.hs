module Amazon.Types
    ( AccessID
    , AccessSecret
    , AssociateTag

    , AmazonConf (..)
    ) where

import           Data.Text
import           Network.HTTP.Conduit

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
