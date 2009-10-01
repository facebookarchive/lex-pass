module NewAst where

import Control.Applicative hiding ((<|>), many)
import Control.Arrow
import Control.Monad
import Data.Char
import Lang.Php.Ast.Common
import Lang.Php.Ast.Expr
import Lang.Php.Ast.Lex
import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language (javaStyle)
import qualified Data.ByteString as BS
import qualified Data.Intercal as IC

upToCharsOrEndParser :: Char -> Char -> Parsec String () (Bool, String)
upToCharsOrEndParser c1 c2 = do
  s <- many (satisfy (/= c1))
  r1Mb <- optionMaybe (char c1)
  second (s ++) <$> case r1Mb of
    Nothing -> return (False, "")
    Just _ -> upToCharsOrEndParserC2 c1 c2

upToCharsOrEndParserC2 :: Char -> Char -> Parsec String () (Bool, String)
upToCharsOrEndParserC2 c1 c2 = do
  r2Mb <- optionMaybe anyChar
  case r2Mb of
    Nothing -> return (False, [c1])
    Just r2 -> if r2 == c2
      then return (True, "")
      else second (c1:) <$> if r2 == c1
        then upToCharsOrEndParserC2 c1 c2
        else second (c2:) <$> upToCharsOrEndParser c1 c2

-- c must be lowercase
charCaseInsens :: Char -> Parsec String () Char
charCaseInsens c = satisfy ((== c) . toLower)

-- given string must be lowercase
stringCaseInsens :: String -> Parsec String () String
stringCaseInsens = mapM charCaseInsens

data Ast = Ast (IC.Intercal NodeTopLevelText NodePhp)
  deriving (Show, Eq)

type NodeTopLevelText = String

data NodePhp = NodePhp {
  nodePhpLabel :: String,
  nodePhpWS1 :: WS,
  nodePhpAst :: PhpAst,
  nodePhpWS2 :: WS,
  nodePhpEndTagExists :: Bool}
  deriving (Show, Eq)

instance Parsable Ast where
  parser = do
    (notEnd, topLevelText) <- upToCharsOrEndParser '<' '?'
    if notEnd
      then do
        (phpLabel, ws1) <-
          liftM2 (,) (stringCaseInsens "php") wsReqParser <|>
          liftM2 (,) ((:[]) <$> char '=' <|> return "") parser
        (phpAst, (ws2, endTagExists)) <- phpAstParser
        -- todo: don't parse full ast on = (just csv list of exprs and stmtEnd)
        Ast rest <- parser
        return . Ast $ IC.Intercal topLevelText
          (NodePhp phpLabel ws1 phpAst ws2 endTagExists) rest
      else return . Ast $ IC.Interend topLevelText

-- for testing
astParser :: Parsec String () Ast
astParser = parser

data PhpAst = PhpAst String
  deriving (Show, Eq)
-- todo

phpAstParser :: Parsec String () (PhpAst, (WS, Bool))
-- todo
phpAstParser = do
  (notEnd, php) <- upToCharsOrEndParser '?' '>'
  if notEnd
    then return (PhpAst php, (WS "", True))
    else return (PhpAst php, (WS "", False))
