{-# LANGUAGE OverloadedStrings #-}

module Amazon.Types.Item
    ( ItemID
    , SearchIndex
    , Condition
    , IdType

    , parseCondition
    , parseIdType
    ) where

import           Data.Text as T

type ItemID  = Text

data SearchIndex = All
                 | Apparel
                 | Appliances
                 | ArtsAndCrafts
                 | Automotive
                 | Baby
                 | Beauty
                 | Blended
                 | Books
                 | Classical
                 | Collectibles
                 | DigitalMusic
                 | Grocery
                 | DVD
                 | Electronics
                 | HealthPersonalCare
                 | HomeGarden
                 | Industrial
                 | Jewelry
                 | KindleStore
                 | Kitchen
                 | LawnGarden
                 | Magazines
                 | Marketplace
                 | Merchants
                 | Miscellaneous
                 | MobileApps
                 | MP3Downloads
                 | Music
                 | MusicalInstruments
                 | MusicTracks
                 | OfficeProducts
                 | OutdoorLiving
                 | PCHardware
                 | PetSupplies
                 | Photo
                 | Shoes
                 | Software
                 | SportingGoods
                 | Tools
                 | Toys
                 | UnboxVideo
                 | VHS
                 | Video
                 | VideoGames
                 | Watches
                 | Wireless
                 | WirelessAccessories
                 deriving (Eq, Show, Read)

data Condition = CAll | CNew | CUsed | CCollectible | CRefurbished deriving (Eq)

instance Show Condition where
    show CAll         = "All"
    show CNew         = "New"
    show CUsed        = "Used"
    show CCollectible = "Collectible"
    show CRefurbished = "Refurbished"

parseCondition :: Text -> Maybe Condition
parseCondition "All"         = Just CAll
parseCondition "New"         = Just CNew
parseCondition "Used"        = Just CUsed
parseCondition "Collectible" = Just CCollectible
parseCondition "Refurbished" = Just CRefurbished
parseCondition _             = Nothing

data IdType = IdASIN | IdSKU | IdUPC | IdEAN | IdISBN deriving (Eq)

instance Show IdType where
    show IdASIN = "ASIN"
    show IdSKU  = "SKU"
    show IdUPC  = "UPC"
    show IdEAN  = "EAN"
    show IdISBN = "ISBN"

parseIdType :: Text -> Maybe IdType
parseIdType "ASIN" = Just IdASIN
parseIdType "SKU"  = Just IdSKU
parseIdType "UPC"  = Just IdUPC
parseIdType "EAN"  = Just IdEAN
parseIdType "ISBN" = Just IdISBN
parseIdType _      = Nothing
