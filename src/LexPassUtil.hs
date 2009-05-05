module LexPassUtil where

import Ast
import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Data.Binary
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

lexPass :: (StmtList -> State (Bool, [String]) StmtList) ->
  FilePath -> FilePath -> Int -> Int -> CanErrStrIO (Bool, [String])
lexPass transf codeDir subPath total cur = do
  io . hPutStrLn stderr $ "Checking (" ++ show cur ++ "/" ++ show total ++
    ") " ++ subPath
  ast <- io $ parseAndCache codeDir subPath
  case runState (transf ast) (False, []) of
    (_,    (False, res)) -> return (False, res)
    (ast', (True,  res)) -> do
      io $ hPutStrLn stderr "- Saving"
      io . writeFile (codeDir </> subPath) . concat . map tokGetVal $
        toToks ast'
      io $ encodeFile (astPath codeDir subPath) ast'
      return (True, res)

--
-- basic transf-building tools
--

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

-- ignores single-statemnt if/etc blocks currently
-- FIXME: switch contents are ignored currently
--        might reconcile StmtList type there before implementing
allStmts :: (WS -> Stmt -> WS -> ([String], Maybe StmtList)) ->
             WS -> Stmt -> WS -> State (Bool, [String]) StmtList
allStmts f wsPre stmt wsPost = case f wsPre stmt wsPost of
  (res, Just stmtMod) -> withState (const (True, res)) $ return stmtMod
  (res, Nothing)      -> withState (second (++ res)) $ case stmt of
    -- todo: class
    {-
    StmtClass x -> single .
      StmtClass $ x {classBlock = doBlock $ classBlock x}
    -}
    StmtFuncDef x -> do
      block' <- doBlock $ funcBlock x
      return . single . StmtFuncDef $ x {funcBlock = block'}
    -- todo: named params to kill much boilerplate here?
    StmtFor ws1 inits conds incrs ws2 (Right block) -> do
      block' <- doBlock $ block
      return . single . StmtFor ws1 inits conds incrs ws2 $ Right block'
    StmtForeach ws1 ws2 expr ws3 ws4 dubArrow ws5 ws6 (Right block) -> do
      block' <- doBlock $ block
      return . single .  StmtForeach ws1 ws2 expr ws3 ws4 dubArrow ws5 ws6 $
        Right block'
    StmtIf ifAndIfelses theElse -> single <$> liftM2 StmtIf
      (IC.mapM ifery return ifAndIfelses) (elsery theElse)
      where
      ifery (ws1, ws2, expr, ws3, ws4, Right block) = do
        block' <- doBlock block
        return (ws1, ws2, expr, ws3, ws4, Right block')
      ifery other = return other
      elsery (Just (ws1, ws2, Right block)) = do
        block' <- doBlock block
        return $ Just (ws1, ws2, Right block')
      elsery other = return other
    {-
    StmtSwitch ws1 ws2 expr ws3 ws4 cases defaultCase ->
      StmtSwitch ws1 ws2 expr ws3 ws4 () ()
    -}
    _ -> return $ single stmt
  where
  single :: Stmt -> StmtList
  single stmt1 = singleStmt wsPre stmt1 wsPost

  doBlock :: Block Stmt -> State (Bool, [String]) (Block Stmt)
  doBlock (Block stmtList) = Block <$> doStmtList stmtList

  doStmtList :: StmtList -> State (Bool, [String]) StmtList
  doStmtList = IC.concatMapM $ allStmts f

singleStmt :: WS -> Stmt -> WS -> StmtList
singleStmt wsPre stmt wsPost = IC.Intercal wsPre stmt $ IC.Interend wsPost

{-
allFuncs :: (WS -> Stmt -> WS -> Maybe StmtList) ->
             WS -> Stmt -> WS -> StmtList
allFuncs f wsPre func wsPost = case f wsPre func wsPost of
  Just funcRepl -> funcRepl
  Nothing       -> case
-}

--
-- behind-the-scenes/lower-level stuff
-- (some of these might be removable after the 2.0 refactor)
--

tokParseKillPos :: String -> Tok
tokParseKillPos l = Tok (head ps) (nlUnesc $ last ps) where
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

parseAndCache :: FilePath -> FilePath -> IO StmtList
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

--
-- eof
--
