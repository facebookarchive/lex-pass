module Transf.Id where

import Lang.Php
import TransfUtil
import qualified Data.Intercal as IC

transfs :: [Transf]
transfs = [
  "id" -:- ftype -?-
  "For testing lex-pass.  Rewrite all files as is."
  -=- argless (lexPass $ changeNothing True),
  "no-op" -:- ftype -?-
  "For testing lex-pass.  Scan all files but do nothing."
  -=- argless (lexPass $ changeNothing False)
  ]

-- optionally pretend we changed something to make this file count and force
-- rewrite.
changeNothing :: Bool -> Ast -> Transformed Ast
changeNothing pretendMod ast =
  if pretendMod
    then pure ast
    else transfNothing

