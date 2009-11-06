module Transf.FuncManip where

import Lang.Php
import TransfUtil
import qualified Data.Intercal as IC

transfs :: [Transf]
transfs = [
  "rename-func <old-func-name> <new-func-name>" -:- ftype -?-
  "Rename a function in all callsites. OO old-func-name not yet supported."
  -=- (\ [oldF, newF] -> lexPass $ renameFunc oldF newF),
  "get-all-defd-funcs" -:- ftype -?-
  "Get a list of all defined functions."
  -=- (\ [] -> lexPass $ getAllDefdFuncs),
  "kill-func-arg <func-name> <arg-n-starting-at-1>" -:- ftype -?-
  "Kill the nth arg of all callsites. OO func-name not yet supported."
  -=- (\ [f, n] -> lexPass . killFuncArg f $ read n),
  "make-public-explicit" -:- ftype -?-
  "Add \"public\" to class functions without an explicit access keyword."
  -=- (\ [] -> lexPass $ makePublicExplicit)]

renameFunc :: String -> String -> Ast -> Transformed Ast
renameFunc oldF newF = modAll $ \ a -> case a of
  ROnlyValFunc (Right (Const [] f)) w args ->
    if f == oldF
      then pure $ ROnlyValFunc (Right $ Const [] newF) w args
      else transfNothing
  _ -> transfNothing

getAllDefdFuncs :: Ast -> Transformed Ast
getAllDefdFuncs = modAll $ \ stmt -> case stmt of
  StmtClass (Class cPre cName@(WSCap _ n _) cExt cImpl cBlock) ->
    (StmtClass . Class cPre cName cExt cImpl <$>) . flip modAll cBlock $
      \ cStmt -> case cStmt of
        CStmtFuncDef _ (Func _ _ f _ _) -> Transformed [n ++ "::" ++ f] Nothing
        _ -> transfNothing
  StmtFuncDef (Func _ _ f _ _) -> Transformed [f] Nothing
  _ -> transfNothing

killFuncArg :: String -> Int -> Ast -> Transformed Ast
killFuncArg f n = modAll $ \ a -> case a of
  ROnlyValFunc c@(Right (Const [] f')) w (Right args) ->
    if f' == f
      then pure $ ROnlyValFunc c w (Right $ take (n - 1) args ++ drop n args)
      else transfNothing
  _ -> transfNothing

isAccessKeyword :: String -> Bool
isAccessKeyword x = x `elem` ["public", "private", "protected"]

makePublicExplicit :: Ast -> Transformed Ast
makePublicExplicit = modAll $ \ cStmt -> case cStmt of
  CStmtFuncDef pre f -> if any (isAccessKeyword . map toLower . fst) pre
    then transfNothing
    else pure $ CStmtFuncDef (("public", [WS " "]):pre) f
  _ -> transfNothing

