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

itemSearch :: Text -> SearchIndex -> [ResponseGroup] -> Condition ->
                Maybe Int -> Maybe Int -> AmazonT (OperationRequest, [Item])
itemSearch keyword si res cond max min = amazonGet "ItemSearch" req xpSearch
    where req      = ItemSearchRequest cond keyword res si max min
          xpSearch = xpElemNodes (nsName "Items") $ xpClean $
            xpFindMatches $ xpElemNodes (nsName "Item") xpItem

itemLookup :: ItemID -> IdType -> [ResponseGroup] -> Condition ->
                AmazonT (OperationRequest, Item)
itemLookup iid iType resGroup cond = amazonGet "ItemLookup" req xpLookup
    where req      = ItemLookupRequest cond iType iid resGroup VPAll
          xpLookup = xpElemNodes (nsName "Items") $ xpClean $
            xpElemNodes (nsName "Item") xpItem
