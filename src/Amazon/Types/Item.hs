{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Amazon.Types.Item
    ( ItemLookupRequest (..)
    , ItemID
    , SearchIndex
    , Condition
    , IdType
    , VariationPage

    , parseCondition
    , parseIdType
    , parseVariationPage

    , xpItemLookupRequest
    , xpItemId
    , xpSearchIndex
    , xpCondition
    , xpIdType
    , xpVariationPage
    ) where

import           Data.Text       as T
import           Data.XML.Pickle
import           Data.XML.Types
import           Prelude         as P

import           Amazon.Types

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

data VariationPage = VPAll
                   deriving (Eq)

instance Show VariationPage where
    show VPAll  = "All"

parseVariationPage :: Text -> VariationPage
parseVariationPage "All"    = VPAll

xpVariationPage :: PU Text VariationPage
xpVariationPage = PU { unpickleTree = return . parseVariationPage
                     , pickleTree   = T.pack . show
                     }

----

data ItemLookupRequest = ItemLookupRequest
        { lookupCondition      :: Condition
        , lookupIdType         :: IdType
        , lookupItemId         :: ItemID
        , lookupResponseGroups :: [ResponseGroup]
        , lookupVariationPage  :: VariationPage
        } deriving (Eq, Show)

xpItemLookupRequest :: PU [Node] ItemLookupRequest
xpItemLookupRequest =
    xpWrap (\(a, b, c, d, e) -> ItemLookupRequest a b c d e)
           (\(ItemLookupRequest a b c d e) -> (a, b, c, d, e)) $
    xp5Tuple
        (xpElemNodes "{http://ecs.amazonaws.com/doc/2011-08-01/}Condition" $
            xpContent xpCondition)
        (xpElemNodes "{http://ecs.amazonaws.com/doc/2011-08-01/}IdType" $
            xpContent xpIdType)
        (xpElemNodes "{http://ecs.amazonaws.com/doc/2011-08-01/}ItemId" $
            xpContent xpItemId)
        (xpList $ xpElemNodes "{http://ecs.amazonaws.com/doc/2011-08-01/}ResponseGroup" $
            xpContent xpResponseGroup)
        (xpElemNodes "{http://ecs.amazonaws.com/doc/2011-08-01/}VariationPage" $
            xpContent xpVariationPage)

instance Parameterize ItemLookupRequest where
    toParams (ItemLookupRequest{..}) =
        [ ("Condition", (T.pack $ show lookupCondition))
        , ("IdType", (T.pack $ show lookupIdType))
        , ("ItemId", (T.pack $ show lookupItemId))
        , ("ResponseGroup", intercalate "," (P.map (T.pack . show) lookupResponseGroups))
        ]
