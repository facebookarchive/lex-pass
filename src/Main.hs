import Control.Applicative
import Control.Concurrent
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
import Text.Regex

import CodeGen.Transf
import LexPassUtil
import Options
import TaskPool
import qualified Config

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
  error $ err ++ usageInfo header options ++
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

transfOnFile :: Options -> Transf -> [String] -> FilePath -> FilePath ->
  Int -> Int -> CanErrStrIO Bool
transfOnFile opts transf args dir file total cur =
  showStRes $ (transfFunc transf) args opts dir file total cur

changeFiles :: Options -> (FilePath -> Int -> Int -> CanErrStrIO Bool) ->
  [FilePath] -> IO ()
changeFiles opts f paths = taskPool (optNumCores opts) .
  map (\ (n, p) -> dieOnErrors $ f p (length paths) n) $ zip [1..] paths

dieOnErrors x = do
  r <- runErrorT x
  case r of
    Left e -> error e
    Right _ -> return ()

killInitialDotSlash ('.':'/':rest) = killInitialDotSlash rest
killInitialDotSlash rest = rest

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  argsOrig <- getArgs
  (opts, transfArgs) <- case getOpt Permute options argsOrig of
    (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) -> usage $ concat errs
  case transfArgs of
    [] -> usage ""
    (transfName:args) -> do
      let
        dir = fromMaybe "." $ optDir opts
        transf = lookupTrans transfName
      subPaths <- map killInitialDotSlash <$> if optFiles opts
        then getContents >>= return . lines
	else Config.sourceFiles (transfTypes transf)
		 (maybe [] (\p -> splitRegex (mkRegex ",") p) (optExclPats opts))
		 dir
      let
        subPaths' = case optStartAtFile opts of
          Nothing -> subPaths
          Just f -> let (pre, rest) = span (/= f') subPaths in case rest of
            [] -> error $ "Couldn't start at file " ++ show f' ++
              " which isn't in the list of files to change."
            _ -> rest ++ pre
            where f' = killInitialDotSlash f
      changeFiles opts (transfOnFile opts transf args dir) subPaths'

