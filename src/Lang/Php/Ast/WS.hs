{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Php.Ast.WS where

import Data.Binary
import Data.Data
import Text.PrettyPrint.GenericPretty

import Common
import Lang.Php.Ast.LexWS
import Parse
import Unparse

-- WSElem

data WSElem = WS String | LineComment Bool String | Comment String
  deriving (Data, Eq, Generic, Show, Typeable)

instance Parse WSElem where
  parse = WS <$> many1 space <|>
    Comment <$> (tokStartCommentP >> upToCharsParser '*' '/') <|> do
      isSlash <- (tokLineCommentP >> return True) <|>
        (tokPoundP >> return False)
      (gotChars, c) <- upToCharsOrEndParser (/= '\n') '?' '>'
      -- hackily put the "?>" back; this should be rare and frowned upon
      -- and i can't believe php works this way with // vs ?>
      when gotChars $ do
        setInput =<< ("?>" ++) <$> getInput
        pos <- getPosition
        setPosition . setSourceColumn pos $ sourceColumn pos - 2
      return $ LineComment isSlash c

instance Unparse WSElem where
  unparse (WS a) = a
  unparse (Comment a) = tokStartComment ++ a ++ tokEndComment
  unparse (LineComment isSlash a) =
    (if isSlash then tokLineComment else tokPound) ++ a

instance Out WSElem

-- WS

type WS = [WSElem]

toWsParser :: Parser a -> Parser (a, WS)
toWsParser p = liftM2 (,) p parse

instance Parse a => Parse (a, WS) where
  parse = toWsParser parse

-- WS2

type WS2 = (WS, WS)

-- | Utility function for unparsing.
w2With :: (Unparse a, Unparse b) => String -> (a, b) -> String
w2With s (w1, w2) = unparse w1 ++ s ++ unparse w2

-- WSCap

data WSCap a = WSCap {
  wsCapPre :: WS,
  wsCapMain :: a,
  wsCapPost :: WS}
  --deriving (Data, Eq, Functor, Generic, Show, Typeable)
  deriving (Data, Eq, Generic, Show, Typeable)

wsCapUnparser :: Unparser a -> Unparser (WSCap a)
wsCapUnparser u (WSCap w1 a w2) = unparse w1 ++ u a ++ unparse w2

instance (Unparse a) => Unparse (WSCap a) where
  unparse = wsCapUnparser unparse

capify :: WS -> (a, WS) -> WSCap a
capify a (b, c) = WSCap a b c

wsToWsCapParser :: Parser (a, WS) -> Parser (WSCap a)
wsToWsCapParser = liftM2 capify parse

instance (Parse (a, WS)) => Parse (WSCap a) where
  parse = wsToWsCapParser parse

instance (Out a) => Out (WSCap a)

wsCapParser :: Parser a -> Parser (WSCap a)
wsCapParser = wsToWsCapParser . toWsParser

-- WSCap2

type WSCap2 a = WSCap (WSCap a)
