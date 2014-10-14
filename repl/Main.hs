{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.Text                  as T
import           Network.HTTP.Conduit
import           System.Environment

import           Amazon
import           Amazon.Item

main :: IO ()
main = do
    conf <- getSandbox
    {-res  <- runAmazonT conf $ itemLookup "B00F0DD0I6" IdASIN [Images, ItemAttributes] CAll-}
    {-res  <- runAmazonT conf $ itemLookup "B00EV97UD6" IdASIN [Images, ItemAttributes] CAll-}
    res  <- runAmazonT conf $ itemSearch "jambox" All [ItemAttributes] CAll Nothing Nothing Nothing
    print res

getSandbox :: IO AmazonConf
getSandbox = do
    accessId     <- getEnv "AWS_ACCESS_ID"
    accessSecret <- getEnv "AWS_ACCESS_SECRET"
    associateTag <- getEnv "AWS_ASSOCIATE_TAG"
    manager      <- newManager conduitManagerSettings
    return $ liveConf manager (T.pack accessId) (Char8.pack accessSecret) (T.pack associateTag)
