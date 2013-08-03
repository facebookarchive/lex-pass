module Transf.AntiPattern where

-- todo:
-- - for assignables-go-right
--   - support more BinOp's with "<=" -> ">=" etc
--   - support joined conditionals on "&&", "||", etc
-- - more anti-pattern correctors

import Lang.Php
import TransfUtil
import Util
import qualified Data.Intercal as IC

transfs :: [Transf]
transfs = [
  "assignables-go-right" -:- ftype -?-
  "\"if ($x == true)\" -> \"if (true == $x)\" etc"
  -=- (\ [] -> lexPass assignablesGoRight),
  "kill-split" -:- ftype -?-
  "split() becomes preg_split()"
  -- TODO: detect non-regex case and go to explode() instead of preg_split()?
  -=- (\ [] -> lexPass killSplit),
  "preg-split-non-regex" -:- ftype -?-
  "preg_split('/a/', ..) becomes explode('a', ..)"
  -=- (\ [] -> lexPass pregSplitNonRegex)
  ]

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
        pure . ROnlyValFunc c' w $ Right (arg0':args)
        where
        c' = Right (Const [] "preg_split")
        arg0' = WSCap w1 (strToArg s') w2
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

pregSplitNonRegex :: Ast -> Transformed Ast
pregSplitNonRegex = modAll $ \ a -> case a of
  ROnlyValFunc _c@(Right (Const [] "preg_split")) w (Right (arg0:args)) ->
    if length args `elem` [1, 2]
      then
        case arg0 of
          WSCap w1 (Left (ExprStrLit (StrLit s))) w2 ->
            if null sRegexPost
              then
                if null sRegexUnits
                  then
                    if length args == 1
                      then pure . ROnlyValFunc cStrSplit w $ Right argsAlone
                      else transfNothing
                  else
                    if any regexUnitIsMeta sRegexUnits
                      then transfNothing
                      else pure . ROnlyValFunc cExplode w $ Right (arg0':args)
              else transfNothing
            where
            (sIsDub, sUnits) = strToUnits s
            (_, (sRegexUnits, sRegexPost)) =
              regexUnits $ map normalizeStrUnit sUnits
            cExplode = Right (Const [] "explode")
            cStrSplit = Right (Const [] "str_split")
            arg0' = WSCap w1 (strToArg s') w2
            s' = strUnitsToStr (sIsDub, map last sRegexUnits)
            argsAlone = onHead (onWSCap1 $ wsStartTransfer w1) args
          _ -> transfNothing
      else transfNothing
  _ -> transfNothing

regexUnitIsMeta :: [String] -> Bool
regexUnitIsMeta [c] = normedStrUnitIsRegexMeta c
regexUnitIsMeta ["\\\\", c] = isAlphaNum . chr $ phpOrd c

-- note that "." and "\x2E" in a PHP str both count as any-char for
-- preg stuff
normedStrUnitIsRegexMeta :: String -> Bool
normedStrUnitIsRegexMeta u = any (== chr (phpOrd u)) "|^$*+?.()[{"

strToArg :: String -> Either Expr b
strToArg = Left . ExprStrLit . StrLit

