module Transf.ExampleGlolbal where

import Ast
import Control.Monad.State
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

addLolsBeforeGlobals :: WS -> Stmt -> WS -> ([String], Maybe StmtList)
addLolsBeforeGlobals wsPre stmt@(StmtGlobal vars StmtEndSemi) wsPost =
  (,) [] . Just .
  IC.Intercal (wsPre ++ [commentTokOf "/* lol */"] ++ lastLine wsPre) stmt $
  IC.Interend wsPost
addLolsBeforeGlobals _ _ _ = ([], Nothing)

lol :: StmtList -> State (Bool, [String]) StmtList
lol = IC.concatMapM $ allStmts addLolsBeforeGlobals
