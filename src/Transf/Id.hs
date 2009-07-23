module Transf.Id where

import Lang.Php
import TransfUtil
import qualified Data.Intercal as IC

transfs :: [Transf]
transfs = [
  "id" -?-
  "For testing lex-pass.  Rewrite all files as is."
  -=- argless (lexPass $ changeNothing True),
  "no-op" -?-
  "For testing lex-pass.  Scan all files but do nothing."
  -=- argless (lexPass $ changeNothing False)
  ]

-- optionally pretend we changed something to make this file count and force
-- rewrite.
changeNothing :: Bool -> StmtList -> Transformed StmtList
changeNothing pretendMod stmtList =
  if pretendMod
    then pure stmtList
    else transfNothing
