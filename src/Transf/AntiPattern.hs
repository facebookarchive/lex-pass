module Transf.AntiPattern where

-- todo:
-- - for assignables-go-right
--   - support more BinOp's with "<=" -> ">=" etc
--   - support joined conditionals on "&&", "||", etc
-- - more anti-pattern correctors

import Lang.Php
import TransfUtil
import qualified Data.Intercal as IC

transfs :: [Transf]
transfs = [
  "assignables-go-right" -:- ftype -?-
  "\"if ($x == true)\" -> \"if (true == $x)\" etc"
  -=- (\ [] -> lexPass $ assignablesGoRight),
  "kill-split" -:- ftype -?-
  "split() becomes preg_split()"
  -- TODO: detect non-regex case and go to explode() instead of preg_split()?
  -=- (\ [] -> lexPass $ killSplit)]

assignablesGoRight :: Ast -> Transformed Ast
assignablesGoRight = modAll . modIfBlockExpr $ modWSCap2 exprLRValToRight

exprLRValToRight :: Expr -> Transformed Expr
exprLRValToRight (ExprBinOp op e1 w e2)
  | op `elem` [BEQ, BNE, BID, BNI] = swapIfGood op
  | op == BLT = swapIfGood BGT
  | op == BGT = swapIfGood BLT
  | op == BLE = swapIfGood BGE
  | op == BGE = swapIfGood BLE
  | otherwise = transfNothing
  where
  swapIfGood op' = if exprIsLRVal e1 && not (exprIsLRVal e2)
    then pure $ ExprBinOp op' e2 w e1
    else transfNothing
exprLRValToRight _ = transfNothing

exprIsLRVal :: Expr -> Bool
exprIsLRVal (ExprRVal (RValLRVal _)) = True
exprIsLRVal _ = False

killSplit :: Ast -> Transformed Ast
killSplit = modAll $ \ a -> case a of
  ROnlyValFunc _c@(Right (Const [] "split")) w (Right (arg0:args)) ->
    case arg0 of
      WSCap w1 (Left (ExprStrLit (StrLit s))) w2 ->
        pure . ROnlyValFunc c' w . Right $ arg0':args
        where
        c' = Right (Const [] "preg_split")
        arg0' = WSCap w1 (Left (ExprStrLit (StrLit s'))) w2
        s' = onTail (onInit $ delimify '/' '\\') s
      _ -> transfNothing
  _ -> transfNothing

delimify :: (Eq a) => a -> a -> [a] -> [a]
delimify delim esc s = [delim] ++ concatMap doEsc s ++ [delim] where
  doEsc c = if c == delim then [esc, c] else [c]

onTail :: ([a] -> [a]) -> [a] -> [a]
onTail f (x:l) = x : f l
onTail _f l = l

onInit :: ([a] -> [a]) -> [a] -> [a]
onInit = reversify . onTail . reversify

