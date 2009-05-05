{-# LANGUAGE TypeSynonymInstances #-}

module Data.Tok where

import Data.Char
import FUtil hiding (choice)
import Text.Parsec hiding (satisfy)
import Text.Parsec.Pos

data Tok = Tok {
  tokGetType :: String,
  tokGetVal :: String
  } deriving (Eq, Show)

instance ParsePos Tok where
  trackTok (Tok tokType tokVal) pos = updatePosString pos tokVal
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

litTokOf :: String -> Tok
litTokOf = Tok "LITERAL"

strTokOf :: String -> Tok
strTokOf = Tok "CONSTANT_ENCAPSED_STRING"

commentTokOf :: String -> Tok
commentTokOf = Tok "COMMENT"

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

tokSelf :: String -> Tok
tokSelf s = Tok (map toUpper s) s

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

tokAbstract = tokSelf "abstract"
tokAnd      = Tok "BOOLEAN_AND"  "&&"
tokAndWd    = Tok "LOGICAL_AND"  "and"
tokArray    = tokSelf "array"
tokAs       = tokSelf "as"
--- note: we don't bother un-@-escaping token types so @@ here instead of @
tokAt       = litTokOf "@@"
tokBacktick = litTokOf "`"
tokBang     = litTokOf "!"
tokBitAndBy = Tok "AND_EQUAL"    "&="
tokBitAnd   = litTokOf "&"
tokBitNot   = litTokOf "~"
tokBitOrBy  = Tok "OR_EQUAL"     "|="
tokBitOr    = litTokOf "|"
tokBitXorBy = Tok "XOR_EQUAL"    "^="
tokBitXor   = litTokOf "^"
tokBreak    = tokSelf "break"
tokCase     = tokSelf "case"
tokCatch    = tokSelf "catch"
tokClass    = tokSelf "class"
tokClone    = tokSelf "clone"
tokCloseTagNL = Tok "CLOSE_TAG" "?>\n"
tokCloseTag = Tok "CLOSE_TAG" "?>"
tokColon    = litTokOf ":"
tokComma    = litTokOf ","
tokConcatBy = Tok "CONCAT_EQUAL" ".="
tokConcat   = litTokOf "."
tokConst    = tokSelf "const"
tokContinue = tokSelf "continue"
tokDeclare  = tokSelf "declare"
tokDecr     = Tok "DEC" "--"
tokDefault  = tokSelf "default"
tokDefine   = tokSelf "define"
tokDie      = Tok "EXIT" "die"
tokDivBy    = Tok "DIV_EQUAL"    "/="
tokDiv      = litTokOf "/"
tokDollar   = litTokOf "$"
tokDo       = tokSelf "do"
tokDubArrow = Tok "DOUBLE_ARROW" "=>"
tokEcho     = tokSelf "echo"
tokElseif   = tokSelf "elseif"
tokElse     = tokSelf "else"
tokEmpty    = tokSelf "empty"
tokEQ       = Tok "IS_EQUAL"     "=="
tokEquals   = litTokOf "="
tokEval     = tokSelf "eval"
tokExit     = tokSelf "exit"
tokExtends  = tokSelf "extends"
tokExtract  = tokSelf "extract"
tokForeach  = tokSelf "foreach"
tokFor      = tokSelf "for"
tokFunction = tokSelf "function"
tokGE       = Tok "IS_GREATER_OR_EQUAL" ">="
tokGlobal   = tokSelf "global"
tokGT       = litTokOf ">"
tokID       = Tok "IS_IDENTICAL" "==="
tokIf       = tokSelf "if"
tokImplements = tokSelf "implements"
tokIncludeOnce = tokSelf "include_once"
tokInclude  = tokSelf "include"
tokIncr     = Tok "INC" "++"
tokInstanceOf = tokSelf "instanceof"
tokInstOf   = tokSelf "instanceof"
tokInterface = tokSelf "interface"
tokIsset    = tokSelf "isset"
tokLBrace   = litTokOf "{"
tokLBracket = litTokOf "["
tokLE       = Tok "IS_SMALLER_OR_EQUAL" "<="
tokList     = tokSelf "list"
tokLParen   = litTokOf "("
tokLT       = litTokOf "<"
tokMember   = Tok "OBJECT_OPERATOR" "->"
tokMinusBy  = Tok "MINUS_EQUAL"  "-="
tokMinus    = litTokOf "-"
tokModBy    = Tok "MOD_EQUAL"    "%="
tokMod      = litTokOf "%"
tokMulBy    = Tok "MUL_EQUAL"    "*="
tokMul      = litTokOf "*"
tokNEOld    = Tok "IS_NOT_EQUAL" "<>"
tokNE       = Tok "IS_NOT_EQUAL" "!="
tokNew      = tokSelf "new"
tokNI       = Tok "IS_NOT_IDENTICAL" "!=="
tokOpenTagWithEcho = Tok "OPEN_TAG_WITH_ECHO" "<?="
tokOr       = Tok "BOOLEAN_OR"   "||"
tokOrWd     = Tok "LOGICAL_OR"   "or"
tokPlusBy   = Tok "PLUS_EQUAL"   "+="
tokPlus     = litTokOf "+"
tokPrint    = tokSelf "print"
tokQuestion = litTokOf "?"
tokQuote    = litTokOf "\""
tokRBrace   = litTokOf "}"
tokRBracket = litTokOf "]"
tokRef      = litTokOf "&"
tokRequireOnce = tokSelf "require_once"
tokRequire  = tokSelf "require"
tokReturn   = tokSelf "return"
tokRParen   = litTokOf ")"
tokSemi     = litTokOf ";"
tokShiftLBy = Tok "SL_EQUAL"     "<<="
tokShiftL   = Tok "SL" "<<"
tokShiftRBy = Tok "SR_EQUAL"     ">>="
tokShiftR   = Tok "SR" ">>"
tokStatic   = tokSelf "static"
tokStatMemb = Tok "DOUBLE_COLON" "::"
tokSwitch   = tokSelf "switch"
tokThrow    = tokSelf "throw"
tokTry      = tokSelf "try"
tokUnset    = tokSelf "unset"
tokWhile    = tokSelf "while"
tokXor      = litTokOf "^"
tokXorWd    = Tok "LOGICAL_XOR"  "xor"
