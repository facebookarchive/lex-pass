{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeSynonymInstances #-}

module Lang.Php.Ast.Common (
  module Control.Applicative,
  module Control.Arrow,
  module Control.Monad,
  module Data.Binary,
  module Data.Char,
  module Data.Data,
  module Data.DeriveTH,
  module Data.List,
  module Data.Maybe,
  module FUtil,
  module Text.Parsec,
  Parse(..), ParseW(..), Unparse(..),
  WS(..), WS2, WSCap(..), capify, wsNoNLParser) where

import Control.Applicative hiding ((<|>), many, optional, Const)
import Control.Arrow
import Control.Monad
import Data.Binary
import Data.Char
import Data.Data hiding (Prefix)
import Data.DeriveTH
import Data.List
import Data.Maybe
import FUtil
import Text.Parsec hiding (parse, choice, uncons)

class Parse a where
  parse :: Parsec String () a

class ParseW a where
  parseW :: Parsec String () (a, WS)

class Unparse a where
  unparse :: a -> String

instance Unparse String where
  unparse s = s

instance (Unparse a, Unparse b) => Unparse (Either a b) where
  unparse (Left a) = unparse a
  unparse (Right a) = unparse a

-- this is just here because WS is in the definition of WParse.  idk.

data WS = WS String
  deriving (Show, Eq, Typeable, Data)

type WS2 = (WS, WS)

instance Parse WS where
  parse = WS <$> many space

instance Unparse WS where
  unparse (WS a) = a

wsReqParser :: Parsec String () WS
wsReqParser = WS <$> many1 space

wsNoNLParser :: Parsec String () WS
wsNoNLParser = WS <$> many (satisfy (\ x -> isSpace x && x /= '\n'))

data WSCap a = WSCap {
  wsCapPre :: WS,
  wsCapMain :: a,
  wsCapPost :: WS}
  deriving (Show, Eq, Typeable, Data)

instance (Parse a) => Parse (WSCap a) where
  parse = liftM3 WSCap parse parse parse

instance (Unparse a) => Unparse (WSCap a) where
  unparse (WSCap a b c) = concat [unparse a, unparse b, unparse c]

capify :: WS -> (a, WS) -> WSCap a
capify a (b, c) = WSCap a b c

$(derive makeBinary ''WS)
$(derive makeBinary ''WSCap)

