module LexPassUtil where

import Common
import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Data.Binary
import Data.Data
import Data.Generics
import FUtil
import HSH
import Lang.Php.Ast
import System.Directory
import System.FilePath
import System.IO
import System.Process
import qualified Data.Intercal as IC

--
-- transf framework
--

data Transf = Transf {
  transfName :: String,
  transfTypes :: [String],
  transfDoc :: String,
  transfArgs :: String,
  transfFunc :: [String] -> FilePath -> FilePath -> Int -> Int ->
    CanErrStrIO (Bool, [String])
  }

data Transformed a = Transformed {
  infoLines :: [String],
  transfResult :: Maybe a
  } deriving (Show)

instance Functor Transformed where
  fmap f t = t {transfResult = fmap f $ transfResult t}

instance Applicative Transformed where
  pure x = Transformed {infoLines = [], transfResult = Just x}
  -- needed? (sensible?) or should we just have Pointed / use own pure
  f <*> t = Transformed {
    infoLines    = infoLines f ++ infoLines t,
    transfResult = transfResult f <*> transfResult t}

(-:-) :: String -> [String] -> (String, [String])
name -:- ftypes = (name, ftypes)

(-?-) :: (String, [String]) -> String -> (String, [String], String)
(name, ftypes) -?- doc = (name, ftypes, doc)

(-=-) :: (String, [String], String) -> ([String] -> FilePath -> FilePath ->
  Int -> Int -> CanErrStrIO (Bool, [String])) -> Transf
(name, ftypes, doc) -=- func = Transf {
  transfName = bareName,
  transfTypes = ftypes,
  transfDoc = doc,
  transfArgs = argInfo,  -- unused currently
  transfFunc = func
  } where (bareName, argInfo) = break (== ' ') name

-- todo: something more graceful here?
argless :: (t -> t1 -> t2) -> [a] -> t -> t1 -> t2
argless f args dir subPath = if null args then f dir subPath
  else error "Expected no arguments."

lexPass :: (Binary a, Parse a, Unparse a) => (a -> Transformed a) ->
  FilePath -> FilePath -> Int -> Int -> CanErrStrIO (Bool, [String])
lexPass transf codeDir subPath total cur = do
  io . hPutStrLn stderr $ "Checking (" ++ show cur ++ "/" ++ show total ++
    ") " ++ subPath
  ast <- io $ parseAndCache codeDir subPath
  case transf ast of
    Transformed {infoLines = infoLines, transfResult = Nothing} ->
      return (False, infoLines)
    Transformed {infoLines = infoLines, transfResult = Just ast'} -> io $ do
      hPutStrLn stderr "- Saving"
      writeFile (codeDir </> subPath) $ unparse ast'
      encodeFile (astPath codeDir subPath) ast'
      return (True, infoLines)

--
-- basic transf-building tools
--

transfNothing :: Transformed a
transfNothing = Transformed {infoLines = [], transfResult = Nothing}

{-
lastIndent :: WS -> (WS, WS)
lastIndent [] = ([], [])
lastIndent ws = case wsTail of
  Tok "WHITESPACE" s ->
    (wsInit ++ wsTokLIfNotNull sMost, wsTokLIfNotNull sAfterLastLine)
    where
    (sMost, sAfterLastLine) = reversifyTup (span (/= '\n')) s
    wsTokLIfNotNull [] = []
    wsTokLIfNotNull x  = [wsTokOf x]
  _ -> (ws, [])
  where
  (wsTail:wsInitRev) = reverse ws
  wsInit = reverse wsInitRev

lastLine :: WS -> WS
lastLine ws = case lastIndent ws of
  (_, [Tok "WHITESPACE" s]) -> [wsTokOf $ '\n':s]
  _ -> [wsTokOf "\n"]

wsSp :: [Tok]
wsSp = [wsTokOf " "]
-}

modIntercal :: (a -> b -> a -> Transformed (IC.Intercal a b)) ->
  IC.Intercal a b -> Transformed (IC.Intercal a b)
modIntercal f ical = case runState (IC.concatMapM f' ical) ([], False) of
  (res, (infoLines, True)) ->
    Transformed {infoLines = infoLines, transfResult = Just res}
  (_, (infoLines, False)) ->
    Transformed {infoLines = infoLines, transfResult = Nothing}
  where
  f' a1 b a2 = case f a1 b a2 of
    Transformed {infoLines = infoLines, transfResult = Just res} ->
      withState (\ (i, _) -> (i ++ infoLines, True)) $ return res
    Transformed {infoLines = infoLines, transfResult = Nothing} ->
      withState (first (++ infoLines)) . return .
      IC.Intercal a1 b $ IC.Interend a2

modMap :: (a -> Transformed a) -> [a] -> Transformed [a]
modMap f xs = case runState (mapM f' xs) ([], False) of
  (res, (infoLines, True)) ->
    Transformed {infoLines = infoLines, transfResult = Just res}
  (_, (infoLines, False)) ->
    Transformed {infoLines = infoLines, transfResult = Nothing}
  where
  f' x = case f x of
    Transformed {infoLines = infoLines, transfResult = Just res} ->
      withState (\ (i, _) -> (i ++ infoLines, True)) $ return res
    Transformed {infoLines = infoLines, transfResult = Nothing} ->
      withState (first (++ infoLines)) $ return x

transformerToState :: (a -> Transformed a) -> a -> State ([String], Bool) a
transformerToState f x = case f x of
  Transformed {infoLines = infoLines, transfResult = Just res} ->
    withState (\ (i, _) -> (i ++ infoLines, True)) $ return res
  Transformed {infoLines = infoLines, transfResult = Nothing} ->
    withState (first (++ infoLines)) $ return x

stateToTransformer :: (a -> State ([String], Bool) a) -> a -> Transformed a
stateToTransformer f x = case runState (f x) ([], False) of
  (res, (infoLines, True)) ->
    Transformed {infoLines = infoLines, transfResult = Just res}
  (_, (infoLines, False)) ->
    Transformed {infoLines = infoLines, transfResult = Nothing}

modAll :: (Typeable a, Data b) => (a -> Transformed a) -> b -> Transformed b
modAll f = stateToTransformer (everywhereM (mkM $ transformerToState f))

--
-- behind-the-scenes/lower-level stuff
-- (some of these might be removable after the 2.0 refactor)
--

astPath :: FilePath -> FilePath -> FilePath
astPath codeDir subPath = codeDir </> ".ast" </> subPath ++ ".ast"

transfModsFile = updateState ((,) True . snd)

-- combine these into AnAst?
parseAndCache :: (Binary a, Parse a, Unparse a) => FilePath -> FilePath -> IO a
parseAndCache codeDir subPath = do
  let
    astFilename = astPath codeDir subPath
    regen = do
      hPutStrLn stderr "- Parsing"
      c <- readFileStrict $ codeDir </> subPath
      case runParser parse () subPath c of
        Left err -> error $ show err
        Right ast -> do
          createDirectoryIfMissing True $ takeDirectory astFilename
          encodeFile astFilename ast
          return ast
  doesFileExist astFilename >>= \ r -> if r
    then do
      mtimeAst  <- getModificationTime astFilename
      mtimeFile <- getModificationTime (codeDir </> subPath)
      if mtimeFile > mtimeAst
        then regen
        else decodeFile astFilename
    else regen

--
-- eof
--

