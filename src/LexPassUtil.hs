module LexPassUtil where

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Data.Ast
import Data.Binary
import Data.Generics
import Data.Tok
import FUtil
import HSH
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Text.Parsec hiding (State)
import qualified Data.Intercal as IC

--
-- transf framework
--

data Transf = Transf {
  transfName :: String,
  transfArgs :: String,
  transfDoc :: String,
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

(-?-) :: String -> String -> (String, String)
name -?- doc = (name, doc)

(-=-) :: (String, String) -> ([String] -> FilePath -> FilePath -> Int -> Int ->
  CanErrStrIO (Bool, [String])) -> Transf
(name, doc) -=- func = Transf {
  transfName = bareName,
  transfArgs = argInfo,
  transfDoc = doc,
  transfFunc = func
  } where (bareName, argInfo) = break (== ' ') name

-- todo: something more graceful here?
argless :: (t -> t1 -> t2) -> [a] -> t -> t1 -> t2
argless f args dir subPath = if null args then f dir subPath
  else error "Expected no arguments."

lexPass :: (StmtLike s) => (InterWS s -> Transformed (InterWS s)) ->
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
      writeFile (codeDir </> subPath) . concat . map tokGetVal $
        toToks ast'
      encodeFile (astPath codeDir subPath) ast'
      return (True, infoLines)

--
-- basic transf-building tools
--

transfNothing :: Transformed a
transfNothing = Transformed {infoLines = [], transfResult = Nothing}

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

tokParseKillPos :: String -> Tok
tokParseKillPos l = Tok (head ps) . nlUnesc $ last ps where
  ps = breaksN (== '\t') 3 l

tokUnparseFakePos :: Tok -> String
tokUnparseFakePos (Tok typ val) = intertabs [typ, "", "", nlEsc val]

astPath :: FilePath -> FilePath -> FilePath
astPath codeDir subPath = codeDir </> ".ast" </> subPath ++ ".ast"

onReadToks :: FilePath -> FilePath -> ([Tok] -> IO a) -> IO a
onReadToks codeDir subPath f = f =<< readToks codeDir subPath

readToks :: FilePath -> FilePath -> IO [Tok]
readToks codeDir subPath = fmap (map tokParseKillPos . lines) . run $
  catFromBS [codeDir </> subPath] -|- "php_lex_stdin"

transfModsFile :: Parsec s (Bool, b) ()
transfModsFile = updateState ((,) True . snd)

parseAndCache :: (StmtLike s) => FilePath -> FilePath -> IO (InterWS s)
parseAndCache codeDir subPath = do
  let
    astFilename = astPath codeDir subPath
    regen = do
      io $ hPutStrLn stderr "- Parsing"
      onReadToks codeDir subPath $ \ toks ->
        case parseAst subPath toks of
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
-- for testing
--

lexWhole :: String -> IO [Tok]
lexWhole = fmap (map tokParseKillPos . lines) . runInp "lex_stdin"

lexFragment :: String -> IO [Tok]
lexFragment = fmap (drop 1) . lexWhole . ("<?php\n" ++)

runInp :: String -> String -> IO String
runInp cmd inp = do
  (pIn, pOut, pErr, pH) <- runInteractiveCommand cmd
  hPutStr pIn inp
  hClose pIn
  waitForProcess pH
  hGetContents pOut

{-
absorbWs :: [Tok] -> InterWS Tok
absorbWs toks = if null rest
  then IC.Interend ws
  else IC.Intercal ws rest1 $ absorbWs restRest
  where
  (ws, rest) = span ((`elem` wsTokTypes) . tokGetType) toks
  rest1:restRest = rest

showFragment :: (Show a) => String -> Parsec [TokWS] () a -> String -> IO ()
showFragment name p s = do
  toks <- lexFragment s
  case runParser p () name . fst . IC.breakEnd $ absorbWs toks of
    Left err -> do
      print err
      putStrLn "on tok stream:"
      mapM_ print toks
    Right res -> print res

showWhole :: String -> String -> IO ()
showWhole name contents = do
  toks <- lexWhole contents
  case parseAst name toks of
    Left err -> do
      print err
      putStrLn "on tok stream:"
      mapM_ print toks
    Right res -> print res

showFile :: String -> IO ()
showFile name = do
  home <- getHomeDirectory
  showWhole name =<< readFileStrict (home </> "www" </> name)
-}

--
-- eof
--
