{-# LANGUAGE DeriveDataTypeable #-}

module Lang.Php.Ast (
  module Lang.Php.Ast.Common,
  module Lang.Php.Ast.Lex,
  module Lang.Php.Ast.Stmt,
  Ast
  ) where

import Control.Applicative hiding ((<|>), many)
import Control.Arrow
import Control.Monad
import Data.Binary.Generic
import Data.Char

import Common
import qualified Data.ByteString as BS
import qualified Data.Intercal as IC
import Lang.Php.Ast.Common
import Lang.Php.Ast.Lex
import Lang.Php.Ast.Stmt

data Ast = Ast TopLevel StmtList
  deriving (Eq, Show, Typeable, Data)

instance Unparse Ast where
  unparse (Ast t s) = unparse t ++ unparse s

instance Parse Ast where
  parse = liftM2 Ast parse stmtListP
