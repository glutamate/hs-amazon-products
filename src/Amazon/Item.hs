{-# LANGUAGE OverloadedStrings #-}

module Amazon.Item
    ( itemSearch
    , itemLookup

    , module Amazon.Types.Item
    ) where

import           Data.Text         as T
import           Data.XML.Pickle

import           Amazon
import           Amazon.Types.Item

itemSearch :: SearchIndex -> Text -> [ResponseGroup] -> Condition ->
                Maybe Int -> Maybe Int -> AmazonT Text
itemSearch si keyword res cond max min = undefined

itemLookup :: ItemID -> IdType -> [ResponseGroup] -> Condition ->
                AmazonT (OperationRequest, Item)
itemLookup iid iType resGroup cond = amazonGet "ItemLookup" req xpLookup
    where req      = ItemLookupRequest cond iType iid resGroup VPAll
          xpLookup = xpElemNodes "{http://ecs.amazonaws.com/doc/2011-08-01/}Items" $
                        xpClean $ xpItem
