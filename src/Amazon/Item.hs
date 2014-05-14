module Amazon.Item
    ( ItemID
    , SearchIndex
    , Condition
    , IdType

    , itemSearch
    , itemLookup
    ) where

import           Amazon
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
                 deriving (Eq, Show)

data Condition = All | New | Used | Collectible | Refurbished deriving (Eq, Show)

data IdType = ASIN | SKU | UPC | EAN | ISBN deriving (Eq, Show)

itemSearch :: SearchIndex -> Text -> [ResponseGroup] -> Maybe Condition ->
                Maybe Int -> Maybe Int -> AmazonT Text
itemSearch si keyword res cond max min = undefined

itemLookup :: ItemID -> IdType -> [ResponseGroup] -> Maybe Condition -> AmazonT Text
itemLookup id iType res cond = undefined
