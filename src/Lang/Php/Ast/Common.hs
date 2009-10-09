{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeSynonymInstances #-}

module Lang.Php.Ast.Common (
  module Common,
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
  Parse(..), ParseW(..), Unparse(..),
  InterWS, WS(..), WS2, WSCap(..), capify, wsNoNLParser) where

import Common
import Control.Applicative hiding ((<|>), many, optional, Const)
import Control.Arrow
import Control.Monad
import Data.Binary
import Data.Char
import Data.Data hiding (Prefix, Infix)
import Data.DeriveTH
import Data.List
import Data.Maybe
import FUtil
import qualified Data.Intercal as IC

class Parse a where
  parse :: Parser a

class ParseW a where
  parseW :: Parser (a, WS)

class Unparse a where
  unparse :: a -> String

instance Unparse String where
  unparse s = s

instance (Unparse a, Unparse b) => Unparse (Either a b) where
  unparse (Left a) = unparse a
  unparse (Right a) = unparse a

data WS = WS String
  deriving (Show, Eq, Typeable, Data)

type WS2 = (WS, WS)

instance Parse WS where
  parse = WS <$> many space

instance Unparse WS where
  unparse (WS a) = a

wsReqParser :: Parser WS
wsReqParser = WS <$> many1 space

wsNoNLParser :: Parser WS
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

type InterWS s = IC.Intercal WS s

$(derive makeBinary ''WS)
$(derive makeBinary ''WSCap)

