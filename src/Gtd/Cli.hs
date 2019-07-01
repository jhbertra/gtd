{-# LANGUAGE DeriveDataTypeable #-}
module Gtd.Cli
    ( defaultMain
    ) where

import System.Console.CmdArgs

data Opts
    = In { items :: [String] }
    deriving (Data,Typeable,Show,Eq)

in_ :: Opts
in_ = In
    { items = def &= args
    } &= help "Add item(s) to the \"in\" list" &= auto

defaultMain :: IO ()
defaultMain = do
    opts <- cmdArgs $
        modes [in_]
        &= help "Getting things done"
        &= program "gtd"
        &= summary "Gtd v1.0"
    print opts