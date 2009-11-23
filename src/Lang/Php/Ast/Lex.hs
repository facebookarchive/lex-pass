{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.Php.Ast.Lex where

import Lang.Php.Ast.Common
import qualified Data.Set as Set

data StrLit = StrLit String
  deriving (Eq, Show, Typeable, Data)

instance Parse StrLit where
  parse = StrLit <$> (
    liftM2 (:) (char '"') (strLitRestParserCurly '"' False) <|>
    liftM2 (:) (char '\'') (strLitRestParser '\'')
    )

instance Unparse StrLit where
  unparse (StrLit a) = a

strLitRestParser :: Char -> Parser String
strLitRestParser end = anyChar >>= \ c -> (c:) <$>
  if c == end then return [] else if c == '\\'
    then liftM2 (:) anyChar (strLitRestParser end)
    else strLitRestParser end

-- "{$a["{$a}"]}" e.g. is a legal single string literal in php..
strLitRestParserCurly :: Char -> Bool -> Parser String
strLitRestParserCurly end haveCurly = anyChar >>= \ c -> (c:) <$>
  if c == end then return [] else if c == '\\'
    then liftM2 (:) anyChar (strLitRestParserCurly end False)
    else
      if c == '{'
        then strLitRestParserCurly end True
        else
          if haveCurly && c == '$'
            then
              liftM2 (++)
                (strLitRestParserCurly '}' False)
                (strLitRestParserCurly end False)
            else strLitRestParserCurly end False

backticksParser :: Parser String
backticksParser = liftM2 (:) (char '`') (strLitRestParserCurly '`' False)

data NumLit = NumLit String
  deriving (Eq, Show, Typeable, Data)

instance Parse NumLit where
  -- could be tighter
  parse = NumLit <$> (liftM2 (++) numStart (ptAndRest <|> return "") <|>
    ptAndRest)
    where
    numStart = liftM2 (:) (oneOf ['0'..'9']) noDecPt
    ptAndRest = liftM2 (:) (char '.') noDecPt
    noDecPt = many . oneOf $ 'x':['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

instance Unparse NumLit where
  unparse (NumLit a) = a

data HereDoc = HereDoc String
  deriving (Eq, Show, Typeable, Data)

instance Parse HereDoc where
  parse = HereDoc <$> do
    ws <- tokHereDocP >> wsNoNLParser
    s <- genIdentifierParser
    nl <- newline
    rest <- hereDocRestParser s
    return (ws ++ s ++ [nl] ++ rest)

instance Unparse HereDoc where
  unparse (HereDoc a) = tokHereDoc ++ a

hereDocRestParser :: String -> Parser String
hereDocRestParser s =
  try (string s <* notFollowedBy (satisfy (\ c -> c /= '\n' && c /= ';'))) <|>
  liftM2 (++) lineParser (hereDocRestParser s)

lineParser :: Parser String
lineParser = liftM2 (++) (many $ satisfy (/= '\n')) ((:[]) <$> newline)

identStartChars :: String
identStartChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']

identEndChars :: String
identEndChars = identStartChars ++ ['0'..'9']

identXmlChars :: String
identXmlChars = identStartChars ++ ['0'..'9'] ++ ['-']

genIdentifierParser :: Parser String
genIdentifierParser =
  liftM2 (:) (satisfy (`elem` identStartChars))
    (many $ satisfy (`elem` identEndChars)) <|>
  concat <$> many1
    (liftM2 (++) tokColonP . many1 $ satisfy (`elem` identXmlChars))

identifierParser :: Parser String
identifierParser = try $ do
  i <- genIdentifierParser
  when (map toLower i `Set.member` reservedWords) $
    fail "Found reserved word when expecting identifier."
  return i

-- must be given lowercase
charCI :: Char -> Parser Char
charCI c = satisfy ((== c) . toLower)

-- must be given lowercase
stringCI :: String -> Parser String
stringCI = mapM charCI

-- idk why but we need an explicit specialized type instead of using (string)
-- directly
s :: String -> Parser String
s = string

nc t cs = try $ s t <* notFollowedBy (oneOf cs)

-- ugly, redo this.. maybe have a minimal lexer stage after all?
tokNot = "!"
tokNotP = nc tokNot "="
tokNE = "!="
tokNEP = nc tokNE "="
tokNI = "!=="
tokNIP = try $ s tokNI
tokDollar = "$"
tokDollarP = s tokDollar
tokMod = "%"
tokModP = nc tokMod "="
tokModBy = "%="
tokModByP = try $ s tokModBy
tokAmp = "&"
tokAmpP = nc tokAmp "&="
tokAnd = "&&"
tokAndP = try $ s tokAnd
tokBitAndBy = "&="
tokBitAndByP = try $ s tokBitAndBy
tokLParen = "("
tokLParenP = s tokLParen
tokRParen = ")"
tokRParenP = s tokRParen
tokMul = "*"
tokMulP = nc tokMul "=/"
tokMulBy = "*="
tokMulByP = try $ s tokMulBy
tokPlus = "+"
tokPlusP = nc tokPlus "+="
tokIncr = "++"
tokIncrP = try $ s tokIncr
tokPlusBy = "+="
tokPlusByP = try $ s tokPlusBy
tokComma = ","
tokCommaP = s tokComma
tokMinus = "-"
tokMinusP = nc tokMinus "-=>"
tokDecr = "--"
tokDecrP = try $ s tokDecr
tokMinusBy = "-="
tokMinusByP = try $ s tokMinusBy
tokArrow = "->"
tokArrowP = try $ s tokArrow
tokConcat = "."
tokConcatP = nc tokConcat "="
tokConcatBy = ".="
tokConcatByP = try $ s tokConcatBy
tokDiv = "/"
tokDivP = nc tokDiv "=*/"
tokDivBy = "/="
tokDivByP = try $ s tokDivBy
tokColon = ":"
tokColonP = nc tokColon ":"
tokDubColon = "::"
tokDubColonP = try $ s tokDubColon
tokSemi = ";"
tokSemiP = s tokSemi
tokLT = "<"
tokLTP = nc tokLT "<=>"
tokShiftL = "<<"
tokShiftLP = nc tokShiftL "<="
tokHereDoc = "<<<"
tokHereDocP = try $ s tokHereDoc
tokShiftLBy = "<<="
tokShiftLByP = try $ s tokShiftLBy
tokLE = "<="
tokLEP = try $ s tokLE
tokNEOld = "<>"
tokNEOldP = try $ s tokNEOld
tokOpenPhp = "<?php"
tokOpenPhpP = try $ s "<?" >> optional (identCI "php")
tokOpenPhpEcho = "<?="
-- no tokOpenPhpEchoP, done manually currently, has weird rules
tokEquals = "="
tokEqualsP = nc tokEquals "=>"
tokEQ = "=="
tokEQP = nc tokEQ "="
tokID = "==="
tokIDP = try $ s tokID
tokDubArrow = "=>"
tokDubArrowP = try $ s tokDubArrow
tokGT = ">"
tokGTP = nc tokGT "=>"
tokGE = ">="
tokGEP = try $ s tokGE
tokShiftR = ">>"
tokShiftRP = nc tokShiftR "="
tokShiftRBy = ">>="
tokShiftRByP = try $ s tokShiftRBy
tokQMark = "?"
tokQMarkP = nc tokQMark ">"
tokClosePhp = "?>"
tokClosePhpP = try $ s tokClosePhp
tokAt = "@"
tokAtP = s tokAt
tokLBracket = "["
tokLBracketP = s tokLBracket
tokRBracket = "]"
tokRBracketP = s tokRBracket
tokXor = "^"
tokXorP = nc tokXor "="
tokXorBy = "^="
tokXorByP = try $ s tokXorBy
tokLBrace = "{"
tokLBraceP = s tokLBrace
tokBitOr = "|"
tokBitOrP = nc tokBitOr "=|"
tokBitOrBy = "|="
tokBitOrByP = try $ s tokBitOrBy
tokOr = "||"
tokOrP = try $ s tokOr
tokRBrace = "}"
tokRBraceP = s tokRBrace
tokBitNot = "~"
tokBitNotP = s tokBitNot

tokAbstract = "abstract"
tokAndWd = "and"
tokArray = "array"
tokAs = "as"
tokBreak = "break"
tokCase = "case"
tokCatch = "catch"
tokClass = "class"
tokClone = "clone"
tokConst = "const"
tokContinue = "continue"
tokDeclare = "declare"
tokDefault = "default"
tokDie = "die"
tokDo = "do"
tokEcho = "echo"
tokElse = "else"
tokElseif = "elseif"
tokEmpty = "empty"
tokEnddeclare = "enddeclare"
tokEndfor = "endfor"
tokEndforeach = "endforeach"
tokEndif = "endif"
tokEndswitch = "endswitch"
tokEndwhile = "endwhile"
tokEval = "eval"
tokExit = "exit"
tokExtends = "extends"
tokFinal = "final"
tokFor = "for"
tokForeach = "foreach"
tokFunction = "function"
tokGlobal = "global"
tokGoto = "goto"
tokIf = "if"
tokImplements = "implements"
tokInclude = "include"
tokIncludeOnce = "include_once"
tokInstanceof = "instanceof"
tokInterface = "interface"
tokIsset = "isset"
tokList = "list"
tokNamespace = "namespace"
tokNew = "new"
tokOrWd = "or"
tokPrint = "print"
tokPrivate = "private"
tokProtected = "protected"
tokPublic = "public"
tokRequire = "require"
tokRequireOnce = "require_once"
tokReturn = "return"
tokStatic = "static"
tokSwitch = "switch"
tokThrow = "throw"
tokTry = "try"
tokUnset = "unset"
tokVar = "var"
tokWhile = "while"
tokXorWd = "xor"

reservedWords :: Set.Set String
reservedWords = Set.fromList [
  tokAbstract,
  tokAndWd,
  tokArray,
  tokAs,
  tokBreak,
  tokCase,
  tokCatch,
  tokClass,
  tokClone,
  tokConst,
  tokContinue,
  tokDeclare,
  tokDefault,
  tokDie,
  tokDo,
  tokEcho,
  tokElse,
  tokElseif,
  tokEmpty,
  tokEnddeclare,
  tokEndfor,
  tokEndforeach,
  tokEndif,
  tokEndswitch,
  tokEndwhile,
  tokEval,
  tokExit,
  tokExtends,
  tokFinal,
  tokFor,
  tokForeach,
  tokFunction,
  tokGlobal,
  tokGoto,
  tokIf,
  tokImplements,
  tokInclude,
  tokIncludeOnce,
  tokInstanceof,
  tokInterface,
  tokIsset,
  tokList,
  tokNamespace,
  tokNew,
  tokOrWd,
  tokPrint,
  tokPrivate,
  tokProtected,
  tokPublic,
  tokRequire,
  tokRequireOnce,
  tokReturn,
  tokStatic,
  tokSwitch,
  tokThrow,
  tokTry,
  tokUnset,
  tokVar,
  tokWhile,
  tokXorWd]

identCI w = try $ do
  i <- genIdentifierParser
  when (map toLower i /= w) $ fail ""
  return i

identsCI w = try $ do
  i <- genIdentifierParser
  when (map toLower i `notElem` w) $ fail ""
  return i

tokAbstractP = identCI tokAbstract
tokAndWdP = identCI tokAndWd
tokArrayP = identCI tokArray
tokAsP = identCI tokAs
tokBreakP = identCI tokBreak
tokCaseP = identCI tokCase
tokCatchP = identCI tokCatch
tokClassP = identCI tokClass
tokCloneP = identCI tokClone
tokConstP = identCI tokConst
tokContinueP = identCI tokContinue
tokDeclareP = identCI tokDeclare
tokDefaultP = identCI tokDefault
tokDieP = identCI tokDie
tokDoP = identCI tokDo
tokEchoP = identCI tokEcho
tokElseifP = identCI tokElseif
tokElseP = identCI tokElse
tokEmptyP = identCI tokEmpty
tokEnddeclareP = identCI tokEnddeclare
tokEndforeachP = identCI tokEndforeach
tokEndforP = identCI tokEndfor
tokEndifP = identCI tokEndif
tokEndswitchP = identCI tokEndswitch
tokEndwhileP = identCI tokEndwhile
tokEvalP = identCI tokEval
tokExitP = identCI tokExit
tokExtendsP = identCI tokExtends
tokFinalP = identCI tokFinal
tokForP = identCI tokFor
tokForeachP = identCI tokForeach
tokFunctionP = identCI tokFunction
tokGlobalP = identCI tokGlobal
tokGotoP = identCI tokGoto
tokIfP = identCI tokIf
tokImplementsP = identCI tokImplements
tokInstanceofP = identCI tokInstanceof
tokInterfaceP = identCI tokInterface
tokIssetP = identCI tokIsset
tokListP = identCI tokList
tokNamespaceP = identCI tokNamespace
tokNewP = identCI tokNew
tokOrWdP = identCI tokOrWd
tokPrintP = identCI tokPrint
tokPrivateP = identCI tokPrivate
tokProtectedP = identCI tokProtected
tokPublicP = identCI tokPublic
tokReturnP = identCI tokReturn
tokStaticP = identCI tokStatic
tokSwitchP = identCI tokSwitch
tokThrowP = identCI tokThrow
tokTryP = identCI tokTry
tokUnsetP = identCI tokUnset
tokVarP = identCI tokVar
tokWhileP = identCI tokWhile
tokXorWdP = identCI tokXorWd

tokCategory = "category"
tokCategoryP = identCI tokCategory
tokChildren = "children"
tokChildrenP = identCI tokChildren
tokAttribute = "attribute"
tokAttributeP = identCI tokAttribute

$(derive makeBinary ''HereDoc)
$(derive makeBinary ''NumLit)
$(derive makeBinary ''StrLit)

