module Lang.Php.Transf where

import Control.Applicative
import Data.Ast
import Lang.Php.Ast
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
      StmtDoWhile (x@DoWhile {doWhileBlock = Right block}) ->
        (\ a -> StmtDoWhile $ x {doWhileBlock = Right a}) <$> doBlock block
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
        elsery (Just (ws1, ws2, Right block)) =
          (\ a -> Just (ws1, ws2, Right a)) <$> doBlock block
        elsery other = pure other
      StmtSwitch x -> StmtSwitch . (\ a -> x {switchCases = a}) <$>
        modMap doCase (switchCases x)
      _ -> transfNothing
    doBlock (Block stmtList) = Block <$> modAllStmts f stmtList
    doCase x = (\ a -> x {caseStmtList = a}) <$> modAllStmts f (caseStmtList x)
    single x = IC.singleton wsPre x wsPost

