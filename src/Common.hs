module Common (module Text.ParserCombinators.Parsec) where

import Text.ParserCombinators.Parsec hiding (State, parse, choice)

type Parser a = CharParser () a
