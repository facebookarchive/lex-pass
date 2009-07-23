-- ast and parsing utility stuff, maybe should be renamed or reorganized

{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeSynonymInstances #-}

module Data.Ast where

import Control.Monad
import Data.Binary
import Data.DeriveTH
import Data.Tok
import Data.Generics
import Text.Parsec
import qualified Data.Intercal as IC

type WS = [Tok]

type InterWS s = IC.Intercal WS s

data WSCap a = WSCap {
  wsCapPre :: WS,
  wsCapMain :: a,
  wsCapPost :: WS
  }
  deriving (Eq, Show, Typeable, Data)

class (ToToks s, Binary s) => StmtLike s where
  parseAst :: FilePath -> [Tok] -> Either ParseError (InterWS s)

class Parsable a where
  parser :: Parsec [TokWS] () a

-- we expect the ws-preserving parsers to usually prepend whitespace to tokens
-- to make the parsing fast (e.g. to keep it LR(1))
type TokWS = (WS, Tok)

class WParsable a where
  wParser :: Parsec [TokWS] () (WS, a)

$(derive makeBinary ''WSCap)
