{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.Php.Ast.Lex where

import Lang.Php.Ast.Common
import qualified Data.Set as Set

data StrLit = StrLit String
  deriving (Eq, Show, Typeable, Data)

instance Parse StrLit where
  parse = StrLit <$> (
    liftM2 (:) (char '"') (strLitRestParser '"') <|>
    liftM2 (:) (char '\'') (strLitRestParser '\'')
    )
instance ParseW StrLit where  -- i want Parse -> ParseW  :(
  parseW = liftM2 (,) parse parse

instance Unparse StrLit where
  unparse (StrLit a) = a

strLitRestParser :: Char -> Parser String
strLitRestParser end = anyChar >>= \ c -> (c:) <$>
  if c == end then return [] else if c == '\\'
    then liftM2 (:) anyChar (strLitRestParser end)
    else strLitRestParser end

backticksParser :: Parser String
backticksParser = liftM2 (:) (char '`') (strLitRestParser '`')

data NumLit = NumLit String
  deriving (Eq, Show, Typeable, Data)

instance Parse NumLit where
  -- could be tighter
  parse = NumLit <$>
    (liftM2 (:) (oneOf ['0'..'9']) . many . oneOf $ 'e':'x':'.':['0'..'9'])
instance ParseW NumLit where  -- i want Parse -> ParseW  :(
  parseW = liftM2 (,) parse parse

instance Unparse NumLit where
  unparse (NumLit a) = a

data HereDoc = HereDoc String
  deriving (Eq, Show, Typeable, Data)

instance Parse HereDoc where
  parse = HereDoc <$> do
    pre <- string "<<<"
    WS ws <- wsNoNLParser
    s <- genIdentifierParser
    nl <- newline
    rest <- hereDocRestParser s
    return (pre ++ ws ++ s ++ [nl] ++ rest)

instance Unparse HereDoc where
  unparse (HereDoc a) = a

hereDocRestParser :: String -> Parser String
hereDocRestParser s =
  try (string $ s ++ ";") <|>
  liftM2 (++) lineParser (hereDocRestParser s)

lineParser :: Parser String
lineParser = liftM2 (++) (many $ satisfy (/= '\n')) ((:[]) <$> newline)

genIdentifierParser :: Parser String
genIdentifierParser = liftM2 (:)
  (satisfy (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['_']))
  genIdentifierRestParser

genIdentifierRestParser :: Parser String
genIdentifierRestParser = many $
  satisfy (`elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9'])

identifierParser :: Parser String
identifierParser = try $ do
  i <- genIdentifierParser
  when (i `Set.member` reservedWords) $
    fail "Found reserved word when expecting identifier."
  return i

-- must be given lowercase
charCI :: Char -> Parser Char
charCI c = satisfy ((== c) . toLower)

-- must be given lowercase
stringCI :: String -> Parser String
stringCI = mapM charCI

tokNot = "!"
tokNE = "!="
tokNI = "!=="
tokDollar = "$"
tokMod = "%"
tokModBy = "%="
tokAmp = "&"
tokAnd = "&&"
tokBitAndBy = "&="
tokLParen = "("
tokRParen = ")"
tokMul = "*"
tokMulBy = "*="
tokPlus = "+"
tokIncr = "++"
tokPlusBy = "+="
tokComma = ","
tokMinus = "-"
tokDecr = "--"
tokMinusBy = "-="
tokArrow = "->"
tokConcat = "."
tokConcatBy = ".="
tokDiv = "/"
tokDivBy = "/="
tokColon = ":"
tokDubColon = "::"
tokLT = "<"
tokShiftL = "<<"
tokShiftLBy = "<<="
tokLE = "<="
tokNEOld = "<>"
tokEquals = "="
tokEQ = "=="
tokID = "==="
tokDubArrow = "=>"
tokGT = ">"
tokGE = ">="
tokShiftR = ">>"
tokShiftRBy = ">>="
tokQMark = "?"
tokAt = "@"
tokLBracket = "["
tokRBracket = "]"
tokXor = "^"
tokXorBy = "^="
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
tokUse = "use"
tokVar = "var"
tokWhile = "while"
tokXorWd = "xor"
tokLBrace = "{"
tokBitOr = "|"
tokBitOrBy = "|="
tokOr = "||"
tokRBrace = "}"
tokBitNot = "~"

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
  tokUse,
  tokVar,
  tokWhile,
  tokXorWd]

-- idk why but we need an explicit specialized type instead of using (string)
-- directly
s :: String -> Parser String
s = string

identCI w = try $ do
  i <- genIdentifierParser
  when (map toLower i /= w) $ fail ""
  return i

identsCI w = try $ do
  i <- genIdentifierParser
  when (map toLower i `notElem` w) $ fail ""
  return i

tokAbstractP = identCI tokAbstract
tokAmpP = s tokAmp
tokAndP = s tokAnd
tokAndWdP = identCI tokAndWd
tokArrayP = identCI tokArray
tokArrowP = s tokArrow
tokAsP = identCI tokAs
tokAtP = s tokAt
tokBitAndByP = s tokBitAndBy
tokBitNotP = s tokBitNot
tokBitOrByP = s tokBitOrBy
tokBitOrP = s tokBitOr
tokBreakP = identCI tokBreak
tokCaseP = identCI tokCase
tokCatchP = identCI tokCatch
tokClassP = identCI tokClass
tokCloneP = identCI tokClone
tokColonP = s tokColon
tokCommaP = s tokComma
tokConcatByP = s tokConcatBy
tokConcatP = s tokConcat
tokConstP = identCI tokConst
tokContinueP = identCI tokContinue
tokDeclareP = identCI tokDeclare
tokDecrP = s tokDecr
tokDefaultP = identCI tokDefault
tokDieP = identCI tokDie
tokDivByP = s tokDivBy
tokDivP = s tokDiv
tokDollarP = s tokDollar
tokDoP = identCI tokDo
tokDubArrowP = s tokDubArrow
tokDubColonP = s tokDubColon
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
tokEQP = s tokEQ
tokEqualsP = s tokEquals
tokEvalP = identCI tokEval
tokExitP = identCI tokExit
tokExtendsP = identCI tokExtends
tokFinalP = identCI tokFinal
tokFunctionP = identCI tokFunction
tokGEP = s tokGE
tokGlobalP = identCI tokGlobal
tokGotoP = identCI tokGoto
tokGTP = s tokGT
tokIDP = s tokID
tokIfP = identCI tokIf
tokImplementsP = identCI tokImplements
tokIncrP = s tokIncr
tokInstanceofP = identCI tokInstanceof
tokInterfaceP = identCI tokInterface
tokIssetP = identCI tokIsset
tokLBraceP = s tokLBrace
tokLBracketP = s tokLBracket
tokLEP = s tokLE
tokListP = identCI tokList
tokLParenP = s tokLParen
tokLTP = s tokLT
tokMinusByP = s tokMinusBy
tokMinusP = s tokMinus
tokModByP = s tokModBy
tokModP = s tokMod
tokMulByP = s tokMulBy
tokMulP = s tokMul
tokNamespaceP = identCI tokNamespace
tokNEOldP = s tokNEOld
tokNEP = s tokNE
tokNewP = s tokNew
tokNIP = s tokNI
tokNotP = s tokNot
tokOrP = s tokOr
tokOrWdP = identCI tokOrWd
tokPlusByP = s tokPlusBy
tokPlusP = s tokPlus
tokPrintP = identCI tokPrint
tokPrivateP = identCI tokPrivate
tokProtectedP = identCI tokProtected
tokPublicP = identCI tokPublic
tokQMarkP = s tokQMark
tokRBraceP = s tokRBrace
tokRBracketP = s tokRBracket
tokReturnP = identCI tokReturn
tokRParenP = s tokRParen
tokShiftLByP = s tokShiftLBy
tokShiftLP = s tokShiftL
tokShiftRByP = s tokShiftRBy
tokShiftRP = s tokShiftR
tokStaticP = identCI tokStatic
tokSwitchP = identCI tokSwitch
tokThrowP = identCI tokThrow
tokTryP = identCI tokTry
tokUnsetP = identCI tokUnset
tokUseP = identCI tokUse
tokVarP = identCI tokVar
tokWhileP = identCI tokWhile
tokXorByP = s tokXorBy
tokXorP = s tokXor
tokXorWdP = identCI tokXorWd

$(derive makeBinary ''HereDoc)
$(derive makeBinary ''NumLit)
$(derive makeBinary ''StrLit)

