import Data.Bifunctor (bimap)
import Data.List
import Data.Text (pack, unpack)
import Data.Time.Calendar
import Debug.Trace
import Test.Hspec
import Test.QuickCheck

import Gtd

main :: IO ()
main = hspec $ do
    
    describe "doInItem" $ do
        it "Is idempotent" $ property prop_doInItem_idempotent
        it "Is id when item not in list" $ property prop_doInItem_idNotInList
        it "Removes the specified item" $ property prop_doInItem

    describe "delegateInItem" $ do
        it "Is idempotent" $ property prop_delegateInItem_idempotent
        it "Is id when item not in list" $ property prop_delegateInItem_idNotInList
        it "Transfers the specified item" $ property prop_delegateInItem

    describe "inItemToNextAction" $ do
        it "Is idempotent" $ property prop_inItemToNextAction_idempotent
        it "Is id when item not in list" $ property prop_inItemToNextAction_idNotInList
        it "Transfers the specified item" $ property prop_inItemToNextAction

data ElementInSet a = ElementInSet a [a] deriving Show

-- doInItem properties
prop_doInItem_idempotent :: ElementInSet String -> Bool
prop_doInItem_idempotent (ElementInSet item list) =
    doInItem item' (doInItem item' items) == doInItem item' items
  where
    item' = pack item
    items = inListfromList $ map (InItem 0 . pack) list

prop_doInItem_idNotInList :: String -> [String] -> Property
prop_doInItem_idNotInList item list =
    notElem item list ==> doInItem item' items == items
  where
    item' = pack item
    items = inListfromList $ map (InItem 0 . pack) list

prop_doInItem :: ElementInSet String -> Bool
prop_doInItem (ElementInSet item list) =
    unInList (doInItem item' items) `haveSame` delete item (nub list)
   where
    item' = pack item
    items = inListfromList $ map (InItem 0 . pack) list

-- delegateInItem properties
prop_delegateInItem_idempotent :: [String] -> ElementInSet String -> Property
prop_delegateInItem_idempotent waitingFor (ElementInSet item inList) =
    null (waitingFor `intersect` inList) ==> actual == expected
  where
    item' = pack item
    inList' = inListfromList $ map (InItem 0 . pack) inList
    waitingFor' = waitingForfromList $ map (\n -> DelegatedAction 0 (pack n) delegate day) waitingFor
    expected = delegateInItem item' delegate day inList' waitingFor'
    actual = 
        let (inList'', waitingFor'') = expected
        in delegateInItem item' delegate day inList'' waitingFor''

prop_delegateInItem_idNotInList :: String -> [String] -> [String] -> Property
prop_delegateInItem_idNotInList item inList waitingFor =
    notElem item inList
        && notElem item waitingFor
        && null (waitingFor `intersect` inList)
        ==> delegateInItem item' delegate day inList' waitingFor' == (inList', waitingFor')
  where
    item' = pack item
    inList' = inListfromList $ map (InItem 0 . pack) inList
    waitingFor' = waitingForfromList $ map (\n -> DelegatedAction 0 (pack n) delegate day) waitingFor

prop_delegateInItem :: ElementInSet String -> [String] -> Property
prop_delegateInItem (ElementInSet item inList) waitingFor =
    null (waitingFor `intersect` inList)
    ==> andT $ bimap (haveSame (delete item (nub inList)) . unInList) (haveSame (item:nub waitingFor) . nub . unWaitingFor) (delegateInItem item' delegate day inList' waitingFor')
  where
    item' = pack item
    inList' = inListfromList $ map (InItem 0 . pack) inList
    waitingFor' = waitingForfromList $ map (\n -> DelegatedAction 0 (pack n) delegate day) waitingFor

-- inItemToNextAction properties
prop_inItemToNextAction_idempotent :: [String] -> ElementInSet String -> Property
prop_inItemToNextAction_idempotent nextActions (ElementInSet item inList) =
    null (nextActions `intersect` inList) ==> actual == expected
  where
    item' = pack item
    inList' = inListfromList $ map (InItem 0 . pack) inList
    nextActions' = nextActionsfromList $ map (Action 0 . pack) nextActions
    expected = inItemToNextAction item' inList' nextActions'
    actual = 
        let (inList'', nextActions'') = expected
        in inItemToNextAction item' inList'' nextActions''

prop_inItemToNextAction_idNotInList :: String -> [String] -> [String] -> Property
prop_inItemToNextAction_idNotInList item inList nextActions =
    notElem item inList
        && notElem item nextActions
        && null (nextActions `intersect` inList)
        ==> inItemToNextAction item' inList' nextActions' == (inList', nextActions')
  where
    item' = pack item
    inList' = inListfromList $ map (InItem 0 . pack) inList
    nextActions' = nextActionsfromList $ map (Action 0 . pack) nextActions

prop_inItemToNextAction :: ElementInSet String -> [String] -> Property
prop_inItemToNextAction (ElementInSet item inList) nextActions =
    null (nextActions `intersect` inList)
    ==> andT $ bimap (haveSame (delete item (nub inList)) . unInList) (haveSame (item:nub nextActions) . nub . unNextActions) (inItemToNextAction item' inList' nextActions')
  where
    item' = pack item
    inList' = inListfromList $ map (InItem 0 . pack) inList
    nextActions' = nextActionsfromList $ map (Action 0 . pack) nextActions

unInList = map (unpack . inItemName) . inListToList
unNextActions = map (unpack . actionName) . nextActionsToList
unWaitingFor = map (unpack . delActionName) . waitingForToList
delegate = pack "delegate"
day = fromGregorian 0 0 0

haveSame :: (Eq a) => [a] -> [a] -> Bool
haveSame x y = null (x \\ y) && null (y \\ x)

andT (a, b) = a && b

instance (Arbitrary a, Eq a) => Arbitrary (ElementInSet a) where
    arbitrary = do
        set <- arbitrary `suchThat` (not . null)
        ElementInSet <$> elements set <*> pure (nub set)