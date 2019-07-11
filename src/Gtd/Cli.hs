{-# LANGUAGE DeriveDataTypeable #-}

module Gtd.Cli
    ( defaultMain
    ) where

import Control.Monad (forM_)
import Data.List (sort)
import Data.Maybe (isJust)
import Data.Text (Text, unpack)
import Database.HDBC (IConnection, commit)
import System.Console.CmdArgs
import System.IO

import Gtd
import Gtd.Database

import qualified Data.Text.IO as TIO
import qualified Data.Text as T

data Opts
    = In { items :: [Text], delete :: Bool, rename :: Bool }
    deriving (Data, Typeable, Show, Eq)

in_ :: Opts
in_ = In
    { items = def &= args
    , delete = def &= help "Delete the items"
    , rename = def &= name "m" &= help "Rename the items"
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
        In [] _ _ -> showInList c
        In items True _ -> deleteFromInList c items
        In items _ True -> renameInInList c items
        In items _ _ -> addToInList c items

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
                        print "here"
                        if exists
                            then errorWithoutStackTrace "gtd: New name conflicts with existing in list item"
                            else updateInItem c $ loaded' { inItemName = newName }
    commit c