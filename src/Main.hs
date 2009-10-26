import Control.Monad
import Control.Monad.Error
import Data.Char
import Data.List
import Data.Maybe
import FUtil
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process

import CodeGen.Transf
import LexPassUtil
import qualified Config

data Options = Options {
  optFiles            :: Bool,
  optOnlyChangedFiles :: Bool,
  optMaxN             :: Maybe Int,
  optDir              :: Maybe String,
  optStartAtFile      :: Maybe String}
  deriving Show

defaultOptions :: Options
defaultOptions = Options {
  optFiles            = False,
  optOnlyChangedFiles = False,
  optMaxN             = Nothing,
  optDir              = Nothing,
  optStartAtFile      = Nothing}

options :: [OptDescr (Options -> Options)]
options = [
  Option "c" ["only-changed-files"]
    (NoArg (\ opts -> opts {optOnlyChangedFiles = True}))
    "Only consider changing files that already\n\
    \have local modifications (NOTE: git-only\n\
    \currently).",
  Option "d" ["dir"]
    (ReqArg (\ d opts -> opts {optDir = Just d}) "<dir>")
    "Top-level directory containing parsable\n\
    \files of interest.  Abstract syntax trees\n\
    \will be cached in top-level .ast/\n\
    \directory.",
  Option "f" ["files"]
    (NoArg (\ opts -> opts {optFiles = True}))
    "Pass a specific list of files to stdin\n\
    \(newline-delimited).",
  Option "n" ["max-n-files"]
    (ReqArg (\ n opts -> opts {optMaxN = Just $ read n}) "<n>")
    "Change no more than <n> files total.",
  Option "s" ["start-at-file"]
    (ReqArg (\ f opts -> opts {optStartAtFile = Just f}) "<file>")
    "Start at a particular file instead of the\n\
    \\"beginning\" of the file list, looping back\n\
    \around to get all files."]

endSpan :: (a -> Bool) -> [a] -> ([a], [a])
endSpan p = uncurry (flip (,)) . bothond reverse . span p . reverse

wordWrap _ [] = []
wordWrap n s = a':wordWrap n b' where
  (a, b) = splitAt n s
  (aToLastWord, aLastWord) = endSpan (not . isSpace) a
  (a', b') = if null b || isSpace (head b)
    then (a, dropWhile isSpace b)
    else (aToLastWord, aLastWord ++ b)

usage :: [Char] -> a
usage err =
  error $ err ++ usageInfo header options ++ err ++
    "Transformers are:\n" ++ intercalate "\n" (sort $ map showTransf transfs)
  where
  header =
    "usage: [<options>] <transformer> <transformer-options ..>\n\
    \Runs <transformer> on lexed codebase files.\n\
    \Options are:"
  showTransf :: Transf -> String
  showTransf t = "" ++ transfName t ++ transfArgs t ++ "\n" ++
    intercalate "\n" (zipWith (++) (repeat "  ") .
    wordWrap 78 $ transfDoc t)

sourceFiles :: [String] -> FilePath -> Bool -> IO [String]
sourceFiles ftypes dir onlyChanged =
  if onlyChanged
    then error "not working right now" --"git-files-modified"
    else Config.sourceFiles ftypes dir

showStRes :: CanErrStrIO (Bool, [String]) -> CanErrStrIO Bool
showStRes f = do
  (ret, st) <- f
  io . putStr $ unlines st
  return ret

lookupTrans :: String -> Transf
lookupTrans name = case filter ((== name) . transfName) transfs of
  [t] -> t
  [] -> error $ "No transformer matched: " ++ name
  _ -> error $ "Serious uh-oh; multiple transformers matched: " ++ name

transfOnFile :: Transf -> [String] -> FilePath -> FilePath -> Int -> Int ->
  CanErrStrIO Bool
transfOnFile transf args dir file total cur =
  showStRes $ (transfFunc transf) args dir file total cur

changeMaxNFiles :: Maybe Int -> Int -> Int ->
  (String -> Int -> Int -> CanErrStrIO Bool) -> [String] -> CanErrStrIO ()
changeMaxNFiles (Just 0) _     _   _ _                    = return ()
changeMaxNFiles _        _     _   _ []                   = return ()
changeMaxNFiles nMb      total cur f (fileName:fileNames) = do
  res <- f fileName total cur
  let
    nMb' = liftM (\ n -> if res then n - 1 else n) nMb
  changeMaxNFiles nMb' total (cur + 1) f fileNames

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  argsOrig <- getArgs
  (opts, transfArgs) <- case getOpt Permute options argsOrig of
    (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> error $ concat errs
  case transfArgs of
    [] -> usage ""
    (transfName:args) -> do
      let
        dir = fromMaybe "." $ optDir opts
        transf = lookupTrans transfName
      subPaths <- if optFiles opts
        then getContents >>= return . lines
        else sourceFiles (transfTypes transf) dir $ optOnlyChangedFiles opts
      ret <- runErrorT $ do
        subPaths' <- case optStartAtFile opts of
          Nothing -> return subPaths
          Just f -> do
            let (pre, rest) = span (/= f) subPaths
            case rest of
              [] -> throwError $ "Couldn't start at file " ++ show f ++
                " which isn't in the list of files to change."
              _ -> return $ rest ++ pre
        changeMaxNFiles (optMaxN opts) (length subPaths') 1
          (transfOnFile transf args dir) subPaths'
      case ret of
        Left err -> hPutStr stderr err
        Right () -> return ()

