module Gtd
    ( InItem(..)
    , InList
    , Action(..)
    , NextActionsList
    , DelegatedAction(..)
    , WaitingForList
    , Project(..)
    , ProjectsList
    , delegateInItem
    , doInItem
    , inItemToNextAction
    , inItemToProject
    , inListfromList
    , inListToList
    , nextActionsfromList
    , nextActionsToList
    , projectsfromList
    , projectsToList
    , waitingForfromList
    , waitingForToList
    ) where

import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Set (Set)
import Data.Time.Calendar (Day)

import qualified Data.Set as S

data InItem = InItem
    { inItemId :: !Int
    , inItemName :: !Text
    }
    deriving (Read, Show, Eq, Ord)

data Action = Action
    { actionId :: !Int
    , actionName :: !Text
    }
    deriving (Read, Show, Eq, Ord)

data DelegatedAction = DelegatedAction
    { delActionId :: !Int
    , delActionName :: !Text
    , delActionDelegate :: !Text
    , delActionDate :: !Day
    }
    deriving (Read, Show, Eq, Ord)

data Project = Project
    { projectId :: !Int
    , projectName :: !Text
    }
    deriving (Read, Show, Eq, Ord)

newtype InList = InList { unInList :: Set InItem } deriving (Eq, Show)
newtype NextActionsList = NextActionsList { unNextActionsList :: Set Action } deriving (Eq, Show)
newtype WaitingForList = WaitingForList { unWaitingForList :: Set DelegatedAction } deriving (Eq, Show)
newtype ProjectsList = ProjectsList { unProjectsList :: Set Project } deriving (Eq, Show)

inListToList :: InList -> [InItem]
inListToList = S.toList . unInList

nextActionsToList :: NextActionsList -> [Action]
nextActionsToList = S.toList . unNextActionsList

waitingForToList :: WaitingForList -> [DelegatedAction]
waitingForToList = S.toList . unWaitingForList

projectsToList :: ProjectsList -> [Project]
projectsToList = S.toList . unProjectsList

inListfromList :: [InItem] -> InList
inListfromList = InList . S.fromList

nextActionsfromList :: [Action] -> NextActionsList
nextActionsfromList = NextActionsList . S.fromList

waitingForfromList :: [DelegatedAction] -> WaitingForList
waitingForfromList = WaitingForList . S.fromList

projectsfromList :: [Project] -> ProjectsList
projectsfromList = ProjectsList . S.fromList

doInItem :: Text -> InList -> InList
doInItem name = InList . S.filter ((/= name) . inItemName) . unInList

inItemToNextAction :: Text -> InList -> NextActionsList -> (InList, NextActionsList)
inItemToNextAction name inList next = bimap InList NextActionsList $ _transfer ((== name) . inItemName) _inItemToAction (unInList inList) (unNextActionsList next)

delegateInItem :: Text -> Text -> Day -> InList -> WaitingForList -> (InList, WaitingForList)
delegateInItem name delegate day inList waitingFor = bimap InList WaitingForList $ _transfer ((== name) . inItemName) (_inItemToDelegatedAction delegate day) (unInList inList) (unWaitingForList waitingFor)

inItemToProject :: Text -> InList -> ProjectsList -> (InList, ProjectsList)
inItemToProject name inList next = bimap InList ProjectsList $ _transfer ((== name) . inItemName) _inItemToProject (unInList inList) (unProjectsList next)

_inItemToAction :: InItem -> Action
_inItemToAction (InItem i n) = Action i n

_inItemToDelegatedAction :: Text -> Day -> InItem -> DelegatedAction
_inItemToDelegatedAction delegate day (InItem i n) = DelegatedAction i n delegate day

_inItemToProject :: InItem -> Project
_inItemToProject (InItem i n) = Project i n

_transfer :: (Ord b) => (a -> Bool) -> (a -> b) -> Set a -> Set b -> (Set a, Set b)
_transfer p f as bs = (S.filter (not . p) as, S.union bs (S.map f (S.filter p as)))