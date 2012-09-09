module Options where

import System.Console.GetOpt

data Options = Options {
  optCacheAsts        :: Bool,
  optNumCores         :: Int,
  optFiles            :: Bool,
  optDir              :: Maybe String,
  optExclPats         :: Maybe String,
  optStartAtFile      :: Maybe String}
  deriving Show

defaultOptions :: Options
defaultOptions = Options {
  optCacheAsts        = True,
  optNumCores         = 1,
  optFiles            = False,
  optDir              = Nothing,
  optExclPats         = Nothing,
  optStartAtFile      = Nothing}

options :: [OptDescr (Options -> Options)]
options = [
  Option "a" ["do-not-cache-asts"]
    (NoArg (\ opts -> opts {optCacheAsts = False}))
    "Do not cache binary dump of ASTs to disk (in .ast/).",
  Option "C" ["num-cores"]
    (ReqArg (\ n opts -> opts {optNumCores = read n}) "<n>")
    "Run n OS threads to optimize for n cores.",
  Option "d" ["dir"]
    (ReqArg (\ d opts -> opts {optDir = Just d}) "<dir>")
    "Top-level directory containing parsable\n\
    \files of interest.  Abstract syntax trees\n\
    \will be cached in top-level .ast/\n\
    \directory.",
  Option "e" ["exclude"]
    (ReqArg (\ d opts -> opts {optExclPats = Just d}) "<patterns>")
    "Exclude files patterns. ",
  Option "f" ["files"]
    (NoArg (\ opts -> opts {optFiles = True}))
    "Pass a specific list of files to stdin\n\
    \(newline-delimited).",
  Option "s" ["start-at-file"]
    (ReqArg (\ f opts -> opts {optStartAtFile = Just f}) "<file>")
    "Start at a particular file instead of the\n\
    \\"beginning\" of the file list, looping back\n\
    \around to get all files."]

