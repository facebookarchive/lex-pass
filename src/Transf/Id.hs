module Transf.Id where

import Lang.Php
import TransfUtil
import qualified Data.Intercal as IC

transfs :: [Transf]
transfs = [
  "id" -:- ftype -?-
  "For testing lex-pass.  Rewrite all files as is."
  -=- argless (lexPass $ changeNothing False True),
  "no-op" -:- ftype -?-
  "For testing lex-pass.  Scan all files but do nothing."
  -=- argless (lexPass $ changeNothing False False),
  "dump-ast" -:- ftype -?-
  "For testing lex-pass.  Dump the AST."
  -=- argless (lexPass $ changeNothing True False),
  "dump-ast-id" -:- ftype -?-
  "For testing lex-pass.  Dump the AST and rewrite as is."
  -=- argless (lexPass $ changeNothing True True)]

-- optionally pretend we changed something to make this file count and force
-- rewrite.
changeNothing :: Bool -> Bool -> Ast -> Transformed Ast
changeNothing dumpAst pretendMod ast =
  Transformed (if dumpAst then [show ast] else []) $
  if pretendMod
    then Just ast
    else Nothing
