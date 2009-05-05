module Transf.Id where

import Ast
import Control.Arrow
import Control.Monad.State
import Data.Tok
import FUtil
import LexPassUtil
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

-- pretend we changed something to make this file count and force write
changeNothing :: Bool -> StmtList -> State (Bool, [String]) StmtList
changeNothing mod stmt = modify (first $ const mod) >> return stmt
