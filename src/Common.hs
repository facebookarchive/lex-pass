{-# LANGUAGE FlexibleInstances #-}

module Common (module Text.ParserCombinators.Parsec, Oper) where

import Control.Applicative
import Control.Monad
import Text.ParserCombinators.Parsec hiding (State, parse, choice)
import Text.ParserCombinators.Parsec.Expr

-- parsec 2 vs 3 stuff

type Oper a = Operator Char () a

instance Applicative (GenParser Char ()) where
  pure = return
  (<*>) = ap

