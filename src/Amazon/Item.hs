{-# LANGUAGE OverloadedStrings #-}

module Amazon.Item
    ( itemSearch
    , itemLookup

    , module Amazon.Types.Item
    ) where

import           Data.Text         as T

import           Amazon
import           Amazon.Types.Item

itemSearch :: SearchIndex -> Text -> [ResponseGroup] -> Maybe Condition ->
                Maybe Int -> Maybe Int -> AmazonT Text
itemSearch si keyword res cond max min = undefined

itemLookup :: ItemID -> IdType -> [ResponseGroup] -> Maybe Condition -> AmazonT Text
itemLookup id iType res cond = undefined
