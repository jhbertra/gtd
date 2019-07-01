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

import Gtd
import Gtd.Database

import qualified Data.Text.IO as TIO

data Opts
    = In { items :: [Text] }
    deriving (Data, Typeable, Show, Eq)

in_ :: Opts
in_ = In
    { items = def &= args
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
        In [] -> showInList c
        In items -> addToInList c items

showInList :: (IConnection c) => c -> IO ()
showInList c = getInItems c >>= mapM_ (TIO.putStrLn . inItemName) . sort

addToInList :: (IConnection c) => c -> [Text] -> IO ()
addToInList c items = do
    forM_ items $ \item -> do
        exists <- isJust <$> getInItem c item
        if exists
            then errorWithoutStackTrace $ "gtd: Item " ++ show item ++ " already in the InList"
            else addInItem c $ InItem 0 item
    commit c