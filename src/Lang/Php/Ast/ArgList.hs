{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.Php.Ast.ArgList where

import Data.Either.Utils
import Lang.Php.Ast.Common
import Lang.Php.Ast.Lex
import qualified Data.Intercal as IC

type ArgList a = Either WS [WSCap a]

argListUnparser :: Unparse a => ArgList a -> String
argListUnparser x =
  tokLParen ++
  either unparse (intercalate tokComma . map unparse) x ++
  tokRParen

-- e.g. ($a, $b, $c) in f($a, $b, $c) or () in f()
argListParser :: Parser (a, WS) -> Parser (Either WS [WSCap a])
argListParser = fmap (map fromRight <$>) .
  genArgListParser False False True True

-- e.g. ($a, $b, $c,) in array($a, $b, $c,) or () in array()
arrListParser :: Parser (a, WS) -> Parser (Either WS ([WSCap a], Maybe WS))
arrListParser = fmap (f <$>) . genArgListParser False True True True
  where
  f args = first (map fromRight) $ case last args of
    Left ws -> (init args, Just ws)
    _ -> (args, Nothing)

-- e.g. ($a, , $c) in list($a, , $c) = array(1, 'bye', 3)
mbArgListParser :: Parser (a, WS) -> Parser (Either WS [Either WS (WSCap a)])
mbArgListParser = genArgListParser True False True True

-- e.g. ($a, $b, $c) in isset($a, $b, $c)
reqArgListParser :: Parser (a, WS) -> Parser [WSCap a]
reqArgListParser = fmap (map fromRight . fromRight) .
  genArgListParser False False False True

-- todo: this can just be separate right?
-- e.g. ($a) in exit($a) or () in exit()
exitListParser :: Parser (a, WS) -> Parser (Either WS (WSCap a))
exitListParser = fmap (fmap (fromRight . head)) .
  genArgListParser False False True False

genArgListParser :: Bool -> Bool -> Bool -> Bool -> Parser (a, WS) ->
  Parser (Either WS [Either WS (WSCap a)])
genArgListParser emptyElemsAllowed finalCommaAllowed singleWSPoss
    overOneArgAllowed p = do
  tokLParenP
  args <- grabArgs emptyElemsAllowed finalCommaAllowed singleWSPoss
    overOneArgAllowed p
  return $ case args of
    [Left ws] -> Left ws
    _ -> Right args

grabArgs :: Bool -> Bool -> Bool -> Bool -> Parser (a, WS) ->
  Parser [Either WS (WSCap a)]
grabArgs emptyElemsAllowed finalCommaAllowed isFirstArgAndWSPoss
    overOneArgAllowed p = do
  ws <- parse
  (arg, canContinue) <-
    if emptyElemsAllowed || finalCommaAllowed || isFirstArgAndWSPoss
    then do
      pResMb <- optionMaybe p
      return $ case pResMb of
        Just pRes -> (Right $ capify ws pRes, overOneArgAllowed)
        _ -> (Left ws, emptyElemsAllowed && overOneArgAllowed)
    else do
      pRes <- p
      return (Right $ capify ws pRes, overOneArgAllowed)
  let
    cont =
      grabArgs emptyElemsAllowed finalCommaAllowed False overOneArgAllowed p
  (arg:) <$> (if canContinue then ((tokCommaP >> cont) <|>) else id)
    (tokRParenP >> return [])

