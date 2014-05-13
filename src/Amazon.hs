{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Amazon
    ( liveConf

    , AmazonT (..)
    , runAmazonT

    , module Amazon.Types
    ) where

import           Amazon.Types
import           Control.Applicative
import           Control.Monad.Base
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Network.HTTP.Conduit

liveConf :: Manager -> AccessID -> AccessSecret -> AssociateTag -> AmazonConf
liveConf = AmazonConf "https://webservices.amazon.com/onca/xml"

newtype AmazonT a = AmazonT
        { unAmazonT :: ResourceT (ReaderT AmazonConf (ErrorT AmazonFailure IO)) a
        } deriving ( Functor, Applicative, Monad, MonadIO, MonadThrow
                   , MonadError AmazonFailure
                   , MonadReader AmazonConf
                   , MonadBase IO
                   , MonadResource )

runAmazonT :: AmazonConf -> AmazonT a -> IO (Either AmazonFailure a)
runAmazonT conf = runErrorT . flip runReaderT conf . runResourceT . unAmazonT
