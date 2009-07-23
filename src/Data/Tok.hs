{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Data.Tok where

import Control.Monad
import Data.Binary
import Data.Char
import Data.DeriveTH
import Data.Generics
import Data.Typeable
import FUtil hiding (choice)
import Text.Parsec hiding (satisfy)
import Text.Parsec.Pos
import qualified Data.Intercal as IC

-- all languages have literals, should we enforce this standardization or make
-- a class for this?

litTokOf :: String -> Tok
litTokOf = Tok "LITERAL"

wsTokOf :: String -> Tok
wsTokOf = Tok "WHITESPACE"

nlUnesc :: String -> String
nlUnesc "" = ""
nlUnesc ('@':'n':rest) = '\n':nlUnesc rest
nlUnesc ('@':'@':rest) = '@':nlUnesc rest
nlUnesc (c:rest) = c:nlUnesc rest

nlEsc :: String -> String
nlEsc = concatMap repls where
  repls c = case c of
    '@' -> "@@"
    '\n' -> "@n"
    c -> [c]

data Tok = Tok {
  tokGetType :: String,
  tokGetVal :: String
  } deriving (Eq, Show, Data, Typeable)

class ToToks s where
  toToks :: s -> [Tok]

instance (ToToks a) => ToToks (Maybe a) where
  toToks = maybe [] toToks

instance (ToToks a, ToToks b) => ToToks (a, b) where
  toToks (a, b) = concat [toToks a, toToks b]

instance (ToToks a, ToToks b, ToToks c) => ToToks (a, b, c) where
  toToks (a, b, c) = concat [toToks a, toToks b, toToks c]

instance (ToToks a, ToToks b, ToToks c, ToToks d) => ToToks (a, b, c, d) where
  toToks (a, b, c, d) = concat [toToks a, toToks b, toToks c, toToks d]

instance (ToToks a, ToToks b, ToToks c, ToToks d, ToToks e) =>
    ToToks (a, b, c, d, e) where
  toToks (a, b, c, d, e) = concat [toToks a, toToks b, toToks c, toToks d]

instance (ToToks a) => ToToks [a] where
  toToks = concatMap toToks

instance (ToToks a, ToToks b) => ToToks (Either a b) where
  toToks (Left a) = toToks a
  toToks (Right b) = toToks b

instance (ToToks a, ToToks b) => ToToks (IC.Intercal a b) where
  toToks (IC.Interend a) = toToks a
  toToks (IC.Intercal a b i) = concat [toToks a, toToks b, toToks i]

instance ParsePos Tok where
  trackTok (Tok tokType tokVal) pos = updatePosString pos tokVal
  -- for debugging
  --trackTok (tokType, tokVal) pos = updatePosString pos "\n"

class (ParsePos t) => Toklike t where
  selfToTok :: t -> Tok

  tokEq :: Tok -> Parsec [t] () t
  tokEq tok = satisfy ((== tok) . selfToTok)

  tokEqNoCase :: Tok -> Parsec [t] () t
  tokEqNoCase (Tok t v) = satisfy
    ((\ (Tok t' v') -> t == t' && map toLower v == map toLower v') . selfToTok)

  tokNEq :: Tok -> Parsec [t] () t
  tokNEq tok = satisfy ((/= tok) . selfToTok)

  tokLit :: String -> Parsec [t] () t
  tokLit = tokEq . litTokOf

  tokType :: String -> Parsec [t] () t
  tokType t = satisfy ((== t) . tokGetType . selfToTok)

  tokNType :: String -> Parsec [t] () t
  tokNType t = satisfy ((/= t) . tokGetType . selfToTok)

  tokTypes :: [String] -> Parsec [t] () t
  tokTypes = choice . map tokType

  toksEq :: [Tok] -> Parsec [t] () [t]
  toksEq = sequence . map tokEq

instance Toklike Tok where
  selfToTok = id

instance ToToks Tok where
  toToks t = [t]

$(derive makeBinary ''Tok)
