module Lang.Php.Transf where

import Control.Applicative
import Lang.Php.Ast
import Lang.Php.Ast.Common
import Lang.Php.Ast.Stmt
import Lang.Php.Ast.StmtTypes
import LexPassUtil
import Numeric
import qualified Data.Intercal as IC

-- ignores single-statement if/etc blocks currently
-- (we _could_ convert them out of single-statement-ness on change)
modAllStmts :: (WS -> Stmt -> WS -> Transformed StmtList) ->
  StmtList -> Transformed StmtList
modAllStmts f = modIntercal $ \ wsPre s wsPost -> case f wsPre s wsPost of
  t@(Transformed {transfResult = Just _}) -> t
  (Transformed {infoLines = ls}) -> single <$> t' where
    t' = t {infoLines = ls ++ infoLines t}
    t = case s of
      StmtDoWhile (x@DoWhile {doWhileBlock = WSCap w1 (Right block) w2}) ->
        (\ a -> StmtDoWhile $ x {doWhileBlock = WSCap w1 (Right a) w2}) <$>
        doBlock block
      StmtFuncDef x ->
        (\ a -> StmtFuncDef $ x {funcBlock = a}) <$> doBlock (funcBlock x)
      StmtFor (x@(For {forBlock = Right block})) -> StmtFor .
        (\ a -> x {forBlock = Right a}) <$> doBlock block
      StmtForeach (x@Foreach {foreachBlock = Right block}) -> StmtForeach .
        (\ a -> x {foreachBlock = Right a}) <$> doBlock block
      StmtIf (If ifAndIfelses theElse) -> StmtIf <$> liftA2 If
        (IC.mapA ifery pure ifAndIfelses) (elsery theElse)
        where
        ifery (x@(IfBlock {ifBlockBlock = Right block})) =
          (\ a -> x {ifBlockBlock = Right a}) <$> doBlock block
        ifery other = pure other
        elsery (Just (ws, Right block)) =
          (\ a -> Just (ws, Right a)) <$> doBlock block
        elsery other = pure other
      StmtSwitch x -> StmtSwitch . (\ a -> x {switchCases = a}) <$>
        modMap doCase (switchCases x)
      _ -> transfNothing
    doBlock (Block stmtList) = Block <$> modAllStmts f stmtList
    doCase x = (\ a -> x {caseStmtList = a}) <$> modAllStmts f (caseStmtList x)
    single x = IC.singleton wsPre x wsPost

modIfBlockExpr :: (WSCap2 Expr -> Transformed (WSCap2 Expr)) ->
  IfBlock -> Transformed IfBlock
modIfBlockExpr t (IfBlock expr block) = flip IfBlock block <$> t expr

modWSCap :: (a -> Transformed a) -> WSCap a -> Transformed (WSCap a)
modWSCap t (WSCap w1 a w2) = (\ b -> WSCap w1 b w2) <$> t a

modWSCap2 :: (a -> Transformed a) -> WSCap2 a -> Transformed (WSCap2 a)
modWSCap2 = modWSCap . modWSCap

lastIndent :: WS -> WS2
lastIndent [] = ([], [])
lastIndent ws = case wsTail of
  WS s ->
    (wsInit ++ wsTokLIfNotNull sMost, wsTokLIfNotNull sAfterLastLine)
    where
    (sMost, sAfterLastLine) = reversifyTup (span (/= '\n')) s
    wsTokLIfNotNull [] = []
    wsTokLIfNotNull x  = [WS x]
  _ -> (ws, [])
  where
  (wsTail:wsInitRev) = reverse ws
  wsInit = reverse wsInitRev

lastLine :: WS -> WS
lastLine ws = case lastIndent ws of
  (_, [WS s]) -> [WS $ '\n':s]
  _ -> [WS "\n"]

onWSCap1 :: (WS -> WS) -> WSCap a -> WSCap a
onWSCap1 f (WSCap w1 a w2) = WSCap (f w1) a w2

onWSCap2 :: (WS -> WS) -> WSCap a -> WSCap a
onWSCap2 f (WSCap w1 a w2) = WSCap w1 a (f w2)

wsStartTransfer :: WS -> WS -> WS
wsStartTransfer a b = takeWhile wsElemIsWS a ++ dropWhile wsElemIsWS b

wsElemIsWS :: WSElem -> Bool
wsElemIsWS (WS _) = True
wsElemIsWS _ = False

strToUnits :: String -> (Bool, [String])
strToUnits ('"':rest) = (,) True . strDubToUnits $ init rest
  where
  strDubToUnits ('\\':c:rest) =
    if c `elem` "nrtv\"\\$"
      then ('\\':[c]) : strDubToUnits rest
      else
        if c == 'x'
          then
            let (ds, rest') = spanUpToN 2 isHexDigit rest in
            ('\\':c:ds) : strDubToUnits rest'
          else
            if isOctDigit c
              then
                let (ds, rest') = spanUpToN 2 isOctDigit rest in
                ('\\':c:ds) : strDubToUnits rest'
              else "\\" : [c] : strDubToUnits rest
  strDubToUnits (c:rest) = [c] : strDubToUnits rest
  strDubToUnits [] = []
strToUnits ('\'':rest) = (,) False . strSingToUnits $ init rest
  where
  strSingToUnits ('\\':'\\':rest) = "\\" : strSingToUnits rest
  strSingToUnits ('\\':'\'':rest) = "'" : strSingToUnits rest
  strSingToUnits (c:rest) = [c] : strSingToUnits rest
  strSingToUnits [] = []

spanUpToN :: Int -> (a -> Bool) -> [a] -> ([a], [a])
spanUpToN 0 _ a = ([], a)
spanUpToN _ f [] = ([], [])
spanUpToN n f a@(x:l) =
  if f x then first (x :) $ spanUpToN (n - 1) f l else ([], a)

strUnitsToStr :: (Bool, [String]) -> String
strUnitsToStr (isDub, s) = [q] ++ concat s ++ [q] where
  q = if isDub then '"' else '\''

-- i.e. "\lol" -> "\\lol"
normalizeStrUnit :: String -> String
normalizeStrUnit "\\" = "\\\\"
normalizeStrUnit ('\\':'X':rest) = normalizeStrUnit ('\\':'x':rest)
normalizeStrUnit ('\\':'x':rest) = "\\x" ++ map toUpper rest
normalizeStrUnit c = c

-- note: only works on normalized str units
regexUnits :: [String] -> (Int, ([[String]], [String]))
regexUnits (c:rest) = (i, regexUnitsDelim i rest) where
  i = phpOrd c
  regexUnitsDelim :: Int -> [String] -> ([[String]], [String])
  regexUnitsDelim delim (b@"\\\\":u:rest) =
    first ([b, u] :) $ regexUnitsDelim delim rest
  regexUnitsDelim delim (u:rest) =
    if phpOrd u == delim
      then ([], rest)
      else first ([u] :) $ regexUnitsDelim delim rest
  regexUnitsDelim _ [] = ([], [])

phpOrd :: String -> Int
phpOrd "\\t" = ord '\t'
phpOrd "\\n" = ord '\n'
phpOrd "\\v" = ord '\v'
phpOrd "\\r" = ord '\r'
phpOrd "\\$" = ord '$'
phpOrd "\\\\" = ord '\\'
phpOrd [c] = ord c
phpOrd ('\\':'x':rest) = fst . head $ readHex rest
phpOrd ('\\':rest) = fst . head $ readOct rest

