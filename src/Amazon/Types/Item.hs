{-# LANGUAGE OverloadedStrings #-}

module Amazon.Types.Item
    ( ItemID
    , SearchIndex
    , Condition
    , IdType

    , parseCondition
    , parseIdType

    , xpItemId
    , xpSearchIndex
    , xpCondition
    , xpIdType
    ) where

import           Data.Text       as T
import           Data.XML.Pickle

type ItemID  = Text

xpItemId :: PU Text ItemID
xpItemId = xpId

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

xpSearchIndex :: PU Text SearchIndex
xpSearchIndex = xpPrim

data Condition = CAll | CNew | CUsed | CCollectible | CRefurbished deriving (Eq)

instance Show Condition where
    show CAll         = "All"
    show CNew         = "New"
    show CUsed        = "Used"
    show CCollectible = "Collectible"
    show CRefurbished = "Refurbished"

parseCondition :: Text -> Condition
parseCondition "All"         = CAll
parseCondition "New"         = CNew
parseCondition "Used"        = CUsed
parseCondition "Collectible" = CCollectible
parseCondition "Refurbished" = CRefurbished

xpCondition :: PU Text Condition
xpCondition = PU { unpickleTree = return . parseCondition
                 , pickleTree   = T.pack . show
                 }

data IdType = IdASIN | IdSKU | IdUPC | IdEAN | IdISBN deriving (Eq)

instance Show IdType where
    show IdASIN = "ASIN"
    show IdSKU  = "SKU"
    show IdUPC  = "UPC"
    show IdEAN  = "EAN"
    show IdISBN = "ISBN"

parseIdType :: Text -> IdType
parseIdType "ASIN" = IdASIN
parseIdType "SKU"  = IdSKU
parseIdType "UPC"  = IdUPC
parseIdType "EAN"  = IdEAN
parseIdType "ISBN" = IdISBN

xpIdType :: PU Text IdType
xpIdType = PU { unpickleTree = return . parseIdType
              , pickleTree   = T.pack . show
              }
