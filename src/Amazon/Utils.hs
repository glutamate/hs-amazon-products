module Amazon.Utils
    ( isASIN
    ) where

import           Data.Char
import           Data.Text as T

isASIN :: Text -> Bool
isASIN x = let l = T.length x == 10
               h = T.head x == 'B'
               a = T.foldl (\b c -> b && isAlphaNum c) True x
            in l && h && a
