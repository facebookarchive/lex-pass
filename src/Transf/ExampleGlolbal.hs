module Transf.ExampleGlolbal where

import Lang.Php
import TransfUtil
import qualified Data.Intercal as IC

transfs :: [Transf]
transfs = [
  "example-glolbal" -:- ftype -?-
  "lex-pass example.  Convert: \
  \\"global $x, $y;\" -> \"/* lol */\\nglobal $x, $y;\""
  -=- argless (lexPass lol)
  ]

addLolsBeforeGlobals :: WS -> Stmt -> WS -> Transformed StmtList
addLolsBeforeGlobals wsPre stmt@(StmtGlobal vars StmtEndSemi) wsPost = pure .
  IC.Intercal (wsPre ++ [Comment "/* lol */"] ++ lastLine wsPre) stmt $
  IC.Interend wsPost
addLolsBeforeGlobals _ _ _ = transfNothing

lol :: Ast -> Transformed Ast
lol = undefined --modAllStmts addLolsBeforeGlobals
