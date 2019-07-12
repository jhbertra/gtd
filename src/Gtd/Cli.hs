{-# LANGUAGE DeriveDataTypeable #-}

module Gtd.Cli
    ( defaultMain
    ) where

import Control.Monad (forM_, (<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy (StateT, execStateT, get, modify)
import Data.List (sort)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time.LocalTime
import Database.HDBC (IConnection, commit, rollback)
import System.Console.CmdArgs
import System.IO
import Text.Read (readMaybe)

import Gtd
import Gtd.Database

import qualified Data.Set as S
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

data Opts
    = In { items :: [Text], delete :: Bool, rename :: Bool, interactive :: Bool }
    deriving (Data, Typeable, Show, Eq)

in_ :: Opts
in_ = In
    { items = def &= args
    , delete = def &= help "Delete the items"
    , rename = def &= name "m" &= help "Rename the items"
    , interactive = def &= name "i" &= help "Interactively process the in list"
    } &= help "Add item(s) to your \"in\" list" &= auto

defaultMain :: IO ()
defaultMain = do
    opts <- cmdArgs $
        modes [in_]
        &= help "Getting things done"
        &= program "gtd"
        &= summary "Gtd v1.0"
    c <- connect
    case opts of
        In _ _ _ True -> processInList c
        In [] _ _ _ -> showInList c
        In items True _ _ -> deleteFromInList c items
        In items _ True _ -> renameInInList c items
        In items _ _ _ -> addToInList c items

showInList :: (IConnection c) => c -> IO ()
showInList c = getInItems c >>= mapM_ (TIO.putStrLn . inItemName) . sort

addToInList :: (IConnection c) => c -> [Text] -> IO ()
addToInList c items = do
    forM_ items $ \item -> do
        exists <- isJust <$> getInItem c item
        if exists
            then errorWithoutStackTrace $ "gtd: Item " ++ show item ++ " is already in the in list"
            else addInItem c $ InItem 0 item
    commit c

deleteFromInList :: (IConnection c) => c -> [Text] -> IO ()
deleteFromInList c items = do
    forM_ items $ \item -> do
        loaded <- getInItem c item
        case loaded of
            Nothing -> errorWithoutStackTrace $ "gtd: Item " ++ show item ++ " is not in the in list"
            Just loaded' -> deleteInItem c loaded'
    commit c

renameInInList :: (IConnection c) => c -> [Text] -> IO ()
renameInInList c items = do
    forM_ items $ \item -> do
        loaded <- getInItem c item
        case loaded of
            Nothing -> errorWithoutStackTrace $ "gtd: Item " ++ show item ++ " is not in the in list"
            Just loaded' -> do
                putStr $ "Enter new name (" ++ show item ++ "): "
                hFlush stdout
                newName <- TIO.getLine
                if T.null newName || newName == item
                    then pure ()
                    else do
                        exists <- isJust <$> getInItem c newName
                        if exists
                            then errorWithoutStackTrace "gtd: New name conflicts with existing in list item"
                            else updateInItem c $ loaded' { inItemName = newName }
    commit c

data ProcessInListOption
    = DoIt
    | NextAction
    | Project
    | Delegate
    | SomeDay
    | Incubate
    | Trash
    | Quit

data InListProcessState = InListProcessState
    { inProcessContinue :: !Bool
    , inProcessInList :: !InList
    , inProcessNextActions :: !NextActionsList
    , inProcessWaitingFor :: !WaitingForList
    }
    deriving (Show)

processInList :: (IConnection c) => c -> IO ()
processInList c = do
    initialState <- loadInitialState
    if null $ inListToList . inProcessInList $ initialState
        then putStrLn "\nIn list empty"
        else do
            state <- execStateT (mapM_ processInItem $ inListToList . inProcessInList $ initialState) initialState
            if inProcessContinue state
                then commitState initialState state
                else rollback c
    putStrLn ""
  where
    loadInitialState = InListProcessState
        True
        <$> fmap inListfromList (getInItems c)
        <*> fmap nextActionsfromList (getActions c)
        <*> fmap waitingForfromList (getDelegatedActions c)

    commitState initialState state = do
        forM_ (deletedInItems initialState state) $ deleteInItem c
        forM_ (addedActions initialState state) $ addAction c
        forM_ (addedDelegatedActions initialState state) $ addDelegatedAction c
        commit c
    
    deletedInItems initialState state =
        let (oldSet, newSet) = mapTuple (S.fromList . inListToList . inProcessInList) (initialState, state)
        in S.toList $ S.difference oldSet newSet
    
    addedActions initialState state =
        let (oldSet, newSet) = mapTuple (S.fromList . nextActionsToList . inProcessNextActions) (initialState, state)
        in S.toList $ S.difference newSet oldSet
    
    addedDelegatedActions initialState state =
        let (oldSet, newSet) = mapTuple (S.fromList . waitingForToList . inProcessWaitingFor) (initialState, state)
        in S.toList $ S.difference newSet oldSet

processInItem :: InItem -> StateT InListProcessState IO ()
processInItem item = do
    continue <- fmap inProcessContinue get
    if continue
        then do
            choice <- liftIO $ getChoice
            handleChoice choice
        else
            pure ()
  where
    getChoice = do
        putStrLn ""
        putStrLn $ "How do you want to handle " ++ (show . inItemName) item ++ "?"
        putStrLn ""
        putStrLn "    1. Do it now (it will take me < 2 min)."
        putStrLn "    2. Put it into my next actions list."
        putStrLn "    3. Make a new project for it."
        putStrLn "    4. Delegate it."
        putStrLn "    5. Do it some day / maybe."
        putStrLn "    6. Sit on it a while longer."
        putStrLn "    7. Trash it."
        putStrLn "    8. Quit."
        putStrLn ""
        putStr "Selection: "
        hFlush stdout
        choice <- fmap (parseChoice <=< readMaybe) getLine
        maybe (putStrLn "" >> putStrLn "That is not a valid option" >> getChoice) pure choice
    
    parseChoice :: Int -> Maybe ProcessInListOption
    parseChoice 1 = Just DoIt
    parseChoice 2 = Just NextAction
    parseChoice 3 = Just Project
    parseChoice 4 = Just Delegate
    parseChoice 5 = Just SomeDay
    parseChoice 6 = Just Incubate
    parseChoice 7 = Just Trash
    parseChoice 8 = Just Quit
    parseChoice _ = Nothing
    
    handleChoice :: ProcessInListOption -> StateT InListProcessState IO ()
    handleChoice DoIt = do
        _ <- liftIO $ do
            putStrLn ""
            putStr "Go do it! (press any key to continue)"
            hFlush stdout
            getLine
        modify $ \s -> s { inProcessInList = doInItem (inItemName item) $ inProcessInList s }
    
    handleChoice NextAction = modify $ \s ->
        let (inList', nextActions') = inItemToNextAction (inItemName item) (inProcessInList s) (inProcessNextActions s)
        in s { inProcessInList = inList', inProcessNextActions = nextActions' }
    
    handleChoice Project = pure ()
    
    handleChoice Delegate = do
        (delegate, day) <- liftIO $ (,) <$> getDelegate <*> getDay
        modify $ \s ->
            let (inList', waitingFor) = delegateInItem (inItemName item) delegate day (inProcessInList s) (inProcessWaitingFor s)
            in s { inProcessInList = inList', inProcessWaitingFor = waitingFor }
    
    handleChoice SomeDay = pure ()
    handleChoice Incubate = pure ()
    handleChoice Trash = modify $ \s -> s { inProcessInList = doInItem (inItemName item) $ inProcessInList s }
    handleChoice Quit = modify $ \s -> s { inProcessContinue = False }

    getDelegate = do
        putStr "Who is this being delegated to? "
        hFlush stdout
        delegate <- TIO.getLine
        if T.null delegate
            then getDelegate
            else pure delegate

    getDay = fmap (localDay . zonedTimeToLocalTime) getZonedTime

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)