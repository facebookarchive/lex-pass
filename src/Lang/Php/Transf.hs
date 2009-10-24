module Lang.Php.Transf where

import Control.Applicative
import Lang.Php.Ast
import Lang.Php.Ast.Common
import Lang.Php.Ast.Stmt
import Lang.Php.Ast.StmtTypes
import LexPassUtil
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

