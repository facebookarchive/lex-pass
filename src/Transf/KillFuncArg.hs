module Transf.KillFuncArg where

import Lang.Php
import TransfUtil
import qualified Data.Intercal as IC

transfs :: [Transf]
transfs = [
  "rename-func <old-func-name> <new-func-name>" -:- ftype -?-
  "Rename a function in all callsites. OO old-func-name not yet supported."
  -=- (\ [oldF, newF] -> lexPass $ renameFunc oldF newF),
  "kill-func-arg <func-name> <arg-n-starting-at-1>" -:- ftype -?-
  "Kill the nth arg of all callsites. OO func-name not yet supported."
  -=- (\ [f, n] -> lexPass . killFuncArg f $ read n)]

renameFunc :: String -> String -> Ast -> Transformed Ast
renameFunc oldF newF = modAll $ \ a -> case a of
  ROnlyValFunc (Right (Const [] f)) w args ->
    if f == oldF
      then pure $ ROnlyValFunc (Right $ Const [] newF) w args
      else transfNothing
  _ -> transfNothing

killFuncArg :: String -> Int -> Ast -> Transformed Ast
killFuncArg f n = modAll $ \ a -> case a of
  ROnlyValFunc c@(Right (Const [] f')) w (Right args) ->
    if f' == f
      then pure $ ROnlyValFunc c w (Right $ take (n - 1) args ++ drop n args)
      else transfNothing
  _ -> transfNothing

