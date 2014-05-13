{-# LANGUAGE OverloadedStrings #-}

module Amazon
    ( liveConf

    , module Amazon.Types
    ) where

import           Amazon.Types
import           Network.HTTP.Conduit

liveConf :: Manager -> AccessID -> AccessSecret -> AssociateTag -> AmazonConf
liveConf = AmazonConf "https://webservices.amazon.com/onca/xml"
