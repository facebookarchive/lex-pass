module Parse where

import Common

class Parse a where
  parse :: Parser a

instance (Parse a) => Parse [a] where
  parse = many parse

instance (Parse a) => Parse (Maybe a) where
  parse = Just <$> parse <|> return Nothing

instance (Parse a, Parse b) => Parse (Either a b) where
  parse = Left <$> parse <|> Right <$> parse
