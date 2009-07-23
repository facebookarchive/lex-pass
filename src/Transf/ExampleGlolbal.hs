module Transf.ExampleGlolbal where

import Lang.Php.Ast
import Lang.Php.Tok
import Lang.Php.Transf
import Control.Applicative
import Data.Ast
import Data.List
import Data.Tok
import FUtil
import LexPassUtil
import qualified Data.Intercal as IC

transfs :: [Transf]
transfs = [
  "example-glolbal" -?-
  "lex-pass example.  Convert: \
  \\"global $x, $y;\" -> \"/* lol */\\nglobal $x, $y;\""
  -=- argless (lexPass lol)
  ]

addLolsBeforeGlobals :: WS -> Stmt -> WS -> Transformed StmtList
addLolsBeforeGlobals wsPre stmt@(StmtGlobal vars StmtEndSemi) wsPost = pure .
  IC.Intercal (wsPre ++ [commentTokOf "/* lol */"] ++ lastLine wsPre) stmt $
  IC.Interend wsPost
addLolsBeforeGlobals _ _ _ = transfNothing

lol :: StmtList -> Transformed StmtList
lol = modAllStmts addLolsBeforeGlobals
