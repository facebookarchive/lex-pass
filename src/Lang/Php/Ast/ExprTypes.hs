{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Lang.Php.Ast.ExprTypes where

import Lang.Php.Ast.Common
import Lang.Php.Ast.Lex
import qualified Data.Intercal as IC

-- Val's are defined to only contain: "$", identifiers, "[Expr]", "[]",
-- "(Exprs)", "${Expr}", "::", "->".  The most important consideration is which
-- ones can be assigned to (LVal's) and which ones can be assigned from
-- (RVal's).  In PHP, most but not all LVal's are also RVal's.

-- Note that this grammar allows "$$a[]->a = 5;" but Zend does not.  However,
-- Zend allows "${$a}[]->a = 5;", and it's not clear what is gained by treating
-- $a and ${..} asymmetrically here.  PHP also allows "${$a}[0]->a = 5" and
-- "$$a[0]->a = 5;".  So we're regarding this as a by-product of the Zend
-- implementation.  In particular, we think they simplify their job by slurping
-- all [Expr?]'s onto Var's and only later analyze things with regard to LVal
-- considerations, simply fataling if something is then awry.
--
-- Modeling that nuance is impractical under the clear division of
-- Var's, LVal's, and RVal's that we desire to make the AST nice for
-- refactoring.

data Val = ValLOnlyVal LOnlyVal | ValROnlyVal ROnlyVal | ValLRVal LRVal
  deriving (Eq, Show, Typeable, Data)

data LVal = LValLOnlyVal LOnlyVal | LValLRVal LRVal
  deriving (Eq, Show, Typeable, Data)

data RVal = RValROnlyVal ROnlyVal | RValLRVal LRVal
  deriving (Eq, Show, Typeable, Data)

data Var =
  -- In php, indexing is oddly coupled very tightly with being a non-dyn var.
  Var        String [((WS, (Bool, WSCap Expr)))] | -- "$a", "$a[0]", "$a[0][0]"
  VarDyn     WS Var          | -- "$$a"
                               -- note: "$$a[0]()->a" == "${$a[0]}()->a"
  VarDynExpr WS (WSCap Expr)   -- "${$a . '_'}"
  deriving (Eq, Show, Typeable, Data)

data DynConst = DynConst [(String, WS2)] Var -- "a::$a"
  deriving (Eq, Show, Typeable, Data)

data LRVal =
  LRValVar  DynConst |
  LRValInd  RVal WS (WSCap Expr) | -- "$a->a[0]"
  LRValMemb RVal WS2 Memb -- $a->a
  deriving (Eq, Show, Typeable, Data)

data LOnlyVal =
  LOnlyValList   WS (Either WS [Either WS (WSCap LVal)]) |
  LOnlyValAppend LVal WS2                 | -- "$a[]"
  LOnlyValInd    LOnlyVal WS (WSCap Expr) | -- "$a[][0]"
  LOnlyValMemb   LOnlyVal WS2 Memb          -- "$a[]->a"
  deriving (Eq, Show, Typeable, Data)

data Const = Const [(String, WS2)] String -- "a::a"
  deriving (Eq, Show, Typeable, Data)

data ROnlyVal =
  ROnlyValConst Const |
  -- "a()", "$a()"
  ROnlyValFunc  (Either LRVal Const) WS (Either WS [WSCap (Either Expr LVal)])
  deriving (Eq, Show, Typeable, Data)

data Memb =
  MembStr  String |
  MembVar  Var    |
  MembExpr (WSCap Expr)
  deriving (Eq, Show, Typeable, Data)

-- Expr's

data Expr =
  ExprArray     WS (Either WS ([WSCap DubArrowMb], Maybe WS)) |
  ExprAssign    (Maybe BinOpBy) LVal WS2 Expr |
  ExprBackticks String |
  ExprBinOp     BinOp Expr WS2 Expr |
  -- we're lazy so just String here instead of like PhpType
  ExprCast      (WSCap String) WS Expr |
  ExprEmpty     WS (WSCap LRVal) |
  ExprEval      WS (WSCap Expr) |
  ExprExit      Bool (Maybe (WS, Either WS (WSCap Expr))) |
  ExprHereDoc   HereDoc |
  ExprInclude   IncOrReq OnceOrNot WS Expr |
  -- true story: "instanceof" takes LRVal's but not non-Const ROnlyVal's..
  ExprInstOf    Expr WS2 (Either LRVal Const) |
  ExprIsset     WS [WSCap LRVal] |
  ExprNew       WS RVal (Maybe (WS, Either WS [WSCap Expr])) |
  ExprNumLit    NumLit |
  ExprParen     (WSCap Expr) |
  ExprPostOp    PostOp Expr WS |
  ExprPreOp     PreOp WS Expr |
  -- note: "list"/"&" is actually more limited
  -- ("list() = &$a;" is nonsyntactic)
  ExprRef       WS (Either Expr Val) |
  ExprRVal      RVal |
  ExprStrLit    StrLit |
  ExprTernaryIf TernaryIf |
  -- FIXME: this fb extension should be separated to a superclass-like Lang?
  ExprXml       Xml
  deriving (Eq, Show, Typeable, Data)

data Xml = Xml String
  (IC.Intercal WS (String, Maybe (WS2, Either StrLit (WSCap Expr))))
  (Maybe ([Either XmlLitOrExpr Xml], Bool))
  deriving (Eq, Show, Typeable, Data)

data XmlLitOrExpr = XmlLit String | XmlExpr (WSCap Expr)
  deriving (Eq, Show, Typeable, Data)

data BinOp = BAnd | BAndWd | BEQ | BGE | BGT | BID | BLE | BLT | BNE |
  -- <> has different precedence than !=
  BNEOld | BNI | BOr | BOrWd | BXorWd | BByable BinOpBy
  deriving (Eq, Show, Typeable, Data)

data BinOpBy = BBitAnd | BBitOr | BConcat | BDiv | BMinus | BMod | BMul |
  BPlus | BShiftL | BShiftR | BXor
  deriving (Eq, Show, Typeable, Data)

data PreOp = PrPrint | PrAt | PrBitNot | PrClone | PrNegate | PrNot | PrPos |
  PrSuppress | PrIncr | PrDecr
  deriving (Eq, Show, Typeable, Data)

data PostOp = PoIncr | PoDecr
  deriving (Eq, Show, Typeable, Data)

data IncOrReq = Inc | Req
  deriving (Eq, Show, Typeable, Data)

data OnceOrNot = Once | NotOnce
  deriving (Eq, Show, Typeable, Data)

data TernaryIf = TernaryIf {
  ternaryIfCond :: Expr,
  ternaryIfWS1  :: WS2,
  ternaryIfThen :: Expr,
  ternaryIfWS2  :: WS2,
  ternaryIfElse :: Expr}
  deriving (Eq, Show, Typeable, Data)

data DubArrowMb = DubArrowMb (Maybe (Expr, WS2)) Expr
  deriving (Eq, Show, Typeable, Data)

$(derive makeBinary ''BinOp)
$(derive makeBinary ''BinOpBy)
$(derive makeBinary ''Const)
$(derive makeBinary ''DubArrowMb)
$(derive makeBinary ''DynConst)
$(derive makeBinary ''Expr)
$(derive makeBinary ''IncOrReq)
$(derive makeBinary ''LOnlyVal)
$(derive makeBinary ''LRVal)
$(derive makeBinary ''LVal)
$(derive makeBinary ''Memb)
$(derive makeBinary ''OnceOrNot)
$(derive makeBinary ''PostOp)
$(derive makeBinary ''PreOp)
$(derive makeBinary ''ROnlyVal)
$(derive makeBinary ''RVal)
$(derive makeBinary ''TernaryIf)
$(derive makeBinary ''Val)
$(derive makeBinary ''Var)
$(derive makeBinary ''Xml)
$(derive makeBinary ''XmlLitOrExpr)

