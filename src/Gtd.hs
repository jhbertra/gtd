module Gtd
    ( InItem(..)
    , InList
    , addToInList
    , empty
    , findById
    , findByName
    , fromList
    , modifyById
    , modifyByName
    , removeFromInList
    , search
    , toList
    ) where

import Data.List (filter, find)
import Data.Text (Text, isInfixOf)

data InItem = InItem
    { inItemId :: !Int
    , inItemName :: !Text
    }
    deriving (Read, Show, Eq, Ord)

newtype InList = InList { unInList :: [InItem] } deriving (Read, Show, Eq)

empty :: InList
empty = InList []

fromList :: [InItem] -> InList
fromList = InList

toList :: InList -> [InItem]
toList = unInList

addToInList :: InItem -> InList -> InList
addToInList item = _modifyInList (item:)

removeFromInList :: InItem -> InList -> InList
removeFromInList item = _modifyInList $ filter (/= item)

findById :: Int -> InList -> Maybe InItem
findById itemId = find ((== itemId) . inItemId) . unInList

findByName :: Text -> InList -> Maybe InItem
findByName itemName = find ((== itemName) . inItemName) . unInList

modifyById :: (InItem -> InItem) -> Int -> InList -> InList
modifyById f itemId = _modifyInList $ map (\i -> if itemId == inItemId i then f i else i)

modifyByName :: (InItem -> InItem) -> Text -> InList -> InList
modifyByName f itemName = _modifyInList $ map (\n -> if itemName == inItemName n then f n else n)

search :: Text -> InList -> InList
search term = _modifyInList $ filter (isInfixOf term . inItemName)

_modifyInList :: ([InItem] -> [InItem]) -> InList -> InList
_modifyInList f = InList . f . unInList