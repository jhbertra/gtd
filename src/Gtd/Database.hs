module Gtd.Database
    ( addAction
    , addDelegatedAction
    , addInItem
    , addProject
    , connect
    , deleteAction
    , deleteDelegatedAction
    , deleteInItem
    , deleteProject
    , getAction
    , getActions
    , getDelegatedAction
    , getDelegatedActions
    , getInItem
    , getInItems
    , getProject
    , getProjects
    , updateAction
    , updateDelegatedAction
    , updateInItem
    , updateProject
    ) where

import Control.Monad (unless)
import Data.Text (Text)
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory

import Gtd

-- INIT

connect :: IO Connection
connect = do
    dir <- getAppUserDataDirectory "gtd"
    exists <- doesDirectoryExist dir
    unless exists $ createDirectory dir
    con <- connectSqlite3 $ dir ++ "/userdata.db"
    prepareDb con
    pure con

prepareDb :: (IConnection c) => c -> IO ()
prepareDb c = do
    tables <- getTables c
    unless ("InItem" `elem` tables) $ do
        _ <- run
            c
            "CREATE TABLE InItem\n\
            \   ( Id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT\n\
            \   , Name TEXT NOT NULL UNIQUE\n\
            \   )"
            []
        pure ()
    unless ("Action" `elem` tables) $ do
        _ <- run
            c
            "CREATE TABLE Action\n\
            \   ( Id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT\n\
            \   , Name TEXT NOT NULL UNIQUE\n\
            \   )"
            []
        pure ()
    unless ("DelegatedAction" `elem` tables) $ do
        _ <- run
            c
            "CREATE TABLE DelegatedAction\n\
            \   ( Id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT\n\
            \   , Name TEXT NOT NULL UNIQUE\n\
            \   , Delegate TEXT NOT NULL\n\
            \   , Date DATE NOT NULL\n\
            \   )"
            []
        pure ()
    unless ("Project" `elem` tables) $ do
        _ <- run
            c
            "CREATE TABLE Project\n\
            \   ( Id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT\n\
            \   , Name TEXT NOT NULL UNIQUE\n\
            \   )"
            []
        pure ()
    commit c

-- CREATE

addInItem ::  (IConnection c) => c -> InItem -> IO InItem
addInItem c (InItem _ name) =
    handleSql (handleError "addInItem") $ do
        _ <- run c "INSERT INTO InItem (Name) VALUES (?)" [toSql name]
        r <- quickQuery' c "SELECT Id FROM InItem WHERE Name = ?" [toSql name]
        case r of
            [[itemId]] -> pure $ InItem (fromSql itemId) name
            x -> fail $ "addInItem: unexpected SQL query result: " ++ show x

addAction ::  (IConnection c) => c -> Action -> IO Action
addAction c (Action _ name) =
    handleSql (handleError "addAction") $ do
        _ <- run c "INSERT INTO Action (Name) VALUES (?)" [toSql name]
        r <- quickQuery' c "SELECT Id FROM Action WHERE Name = ?" [toSql name]
        case r of
            [[actionId]] -> pure $ Action (fromSql actionId) name
            x -> fail $ "addAction: unexpected SQL query result: " ++ show x

addDelegatedAction ::  (IConnection c) => c -> DelegatedAction -> IO DelegatedAction
addDelegatedAction c (DelegatedAction _ name delegate date) =
    handleSql (handleError "addDelegatedAction") $ do
        _ <- run c "INSERT INTO DelegatedAction (Name, Delegate, Date) VALUES (?, ?, ?)" [toSql name, toSql delegate, toSql date]
        r <- quickQuery' c "SELECT Id FROM DelegatedAction WHERE Name = ?" [toSql name]
        case r of
            [[delegatedActionId]] -> pure $ DelegatedAction (fromSql delegatedActionId) name delegate date
            x -> fail $ "addDelegatedAction: unexpected SQL query result: " ++ show x

addProject ::  (IConnection c) => c -> Project -> IO Project
addProject c (Project _ name) =
    handleSql (handleError "addProject") $ do
        _ <- run c "INSERT INTO Project (Name) VALUES (?)" [toSql name]
        r <- quickQuery' c "SELECT Id FROM Project WHERE Name = ?" [toSql name]
        case r of
            [[actionId]] -> pure $ Project (fromSql actionId) name
            x -> fail $ "addProject: unexpected SQL query result: " ++ show x

-- READ

getInItem ::  (IConnection c) => c -> Text -> IO (Maybe InItem)
getInItem = getOne "getInItem" mapInItem
    "SELECT\n\
    \   Id,\n\
    \   Name\n\
    \FROM InItem\n"

getInItems ::  (IConnection c) => c -> IO [InItem]
getInItems = getAll "getInItems" mapInItem
    "SELECT\n\
    \   Id,\n\
    \   Name\n\
    \FROM InItem\n"

getAction ::  (IConnection c) => c -> Text -> IO (Maybe Action)
getAction = getOne "getAction" mapAction
    "SELECT\n\
    \   Id,\n\
    \   Name\n\
    \FROM Action\n"

getActions ::  (IConnection c) => c -> IO [Action]
getActions = getAll "getActions" mapAction
    "SELECT\n\
    \   Id,\n\
    \   Name\n\
    \FROM Action\n"

getDelegatedAction ::  (IConnection c) => c -> Text -> IO (Maybe DelegatedAction)
getDelegatedAction = getOne "getDelegatedAction" mapDelegatedAction
    "SELECT\n\
    \   Id,\n\
    \   Name,\n\
    \   Delegate,\n\
    \   Date\n\
    \FROM DelegatedAction\n"

getDelegatedActions ::  (IConnection c) => c -> IO [DelegatedAction]
getDelegatedActions = getAll "getDelegatedActions" mapDelegatedAction
    "SELECT\n\
    \   Id,\n\
    \   Name,\n\
    \   Delegate,\n\
    \   Date\n\
    \FROM DelegatedAction\n"

getProject ::  (IConnection c) => c -> Text -> IO (Maybe Project)
getProject = getOne "getProject" mapProject
    "SELECT\n\
    \   Id,\n\
    \   Name\n\
    \FROM Project\n"

getProjects ::  (IConnection c) => c -> IO [Project]
getProjects = getAll "getProjects" mapProject
    "SELECT\n\
    \   Id,\n\
    \   Name\n\
    \FROM Project\n"

-- UPDATE

updateInItem ::  (IConnection c) => c -> InItem -> IO ()
updateInItem c (InItem itemId name) =
    handleSql (handleError "updateInItem") $
        run c "UPDATE InItem SET Name = ? WHERE Id = ?" [toSql name, toSql itemId]
        >> pure ()

updateAction ::  (IConnection c) => c -> Action -> IO ()
updateAction c (Action actionId name) =
    handleSql (handleError "updateAction") $
        run c "UPDATE Action SET Name = ? WHERE Id = ?" [toSql name, toSql actionId]
        >> pure ()

updateDelegatedAction ::  (IConnection c) => c -> DelegatedAction -> IO ()
updateDelegatedAction c (DelegatedAction actionId name delegate date) =
    handleSql (handleError "updateDelegatedAction") $
        run c "UPDATE DelegatedAction SET Name = ?, Delegate = ?, Date = ? WHERE Id = ?" [toSql name, toSql delegate, toSql date, toSql actionId]
        >> pure ()

updateProject ::  (IConnection c) => c -> Project -> IO ()
updateProject c (Project actionId name) =
    handleSql (handleError "updateProject") $
        run c "UPDATE Project SET Name = ? WHERE Id = ?" [toSql name, toSql actionId]
        >> pure ()

-- DELETE

deleteInItem ::  (IConnection c) => c -> InItem -> IO ()
deleteInItem c (InItem itemId _) =
    handleSql (handleError "deleteInItem") $
        run c "DELETE FROM InItem WHERE Id = ?" [toSql itemId]
        >> pure ()

deleteAction ::  (IConnection c) => c -> Action -> IO ()
deleteAction c (Action actionId _) =
    handleSql (handleError "deleteAction") $
        run c "DELETE FROM Action WHERE Id = ?" [toSql actionId]
        >> pure ()

deleteDelegatedAction ::  (IConnection c) => c -> DelegatedAction -> IO ()
deleteDelegatedAction c (DelegatedAction actionId _ _ _) =
    handleSql (handleError "deleteDelegatedAction") $
        run c "DELETE FROM DelegatedAction WHERE Id = ?" [toSql actionId]
        >> pure ()

deleteProject ::  (IConnection c) => c -> Project -> IO ()
deleteProject c (Project actionId _) =
    handleSql (handleError "deleteProject") $
        run c "DELETE FROM Project WHERE Id = ?" [toSql actionId]
        >> pure ()

mapInItem :: [SqlValue] -> InItem
mapInItem [itemId, itemName] = InItem (fromSql itemId) (fromSql itemName)
mapInItem x = error $ "mapInItem: wrong column count: " ++ show (length x)

mapAction :: [SqlValue] -> Action
mapAction [actionId, actionName] = Action (fromSql actionId) (fromSql actionName)
mapAction x = error $ "mapAction: wrong column count: " ++ show (length x)

mapDelegatedAction :: [SqlValue] -> DelegatedAction
mapDelegatedAction [dActionId, dActionName, dActionDelegate, dActionDate] =
    DelegatedAction (fromSql dActionId) (fromSql dActionName) (fromSql dActionDelegate) (fromSql dActionDate)
mapDelegatedAction x = error $ "mapDelegatedAction: wrong column count: " ++ show (length x)

mapProject :: [SqlValue] -> Project
mapProject [projectId, projectName] = Project (fromSql projectId) (fromSql projectName)
mapProject x = error $ "mapProject: wrong column count: " ++ show (length x)

handleError :: String -> SqlError -> IO a
handleError name e = fail $ name ++ ": SQL error: " ++ show e

getOne
    :: (IConnection c)
    => String
    -> ([SqlValue] -> a)
    -> String
    -> c
    -> Text
    -> IO (Maybe a)
getOne fname f q c name =
    handleSql (handleError fname) (listToMaybe . map f <$> quickQuery' c (q ++ " WHERE Name = ?") [toSql name])

getAll :: (IConnection c) => String -> ([SqlValue] -> a) -> String -> c -> IO [a]
getAll fname f q c = handleSql (handleError fname) $ map f <$> quickQuery' c q []