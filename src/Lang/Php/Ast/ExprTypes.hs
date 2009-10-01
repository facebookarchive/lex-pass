{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.Php.Ast.ExprTypes where

import Lang.Php.Ast.Common
import Lang.Php.Ast.Lex
import qualified Data.Intercal as IC

-- Val's are defined to only contain: "$", identifiers, "[Expr]", "[]", "()",
-- "${Expr}", "::", "->".  The most important consideration is which ones
-- can be assigned to (LVal's) and which ones can be assigned from (RVal's).
-- In php, most but not all LVal's are also RVal's.

-- Note that this grammar allows "$$a[]->a = 5;" but Zend does not.  However,
-- I cannot see why not since Zend allows "${$a}[]->a = 5;", and it's not
-- clear what is gained by treating $a and ${..} asymmetrically here.
-- Php also allows "${$a}[0]->a = 5" and "$$a[0]->a = 5;".  So I'm regarding
-- this as a quirk (more specifically, a theoretically pointless limitation)
-- that is a by-product of their implementation.  In particular, I think they
-- simplify their job by slurping all [Expr?]'s onto Var's and only later
-- analyze things with regard to LVal considerations, simply fataling if
-- something is then arwy.
--
-- I think think modeling that nuance is impractical in the cleaner division of
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
  Var        String [(WS, WSCap Expr)] | -- "$a", "$a[0]", "$a[0][0]"
  VarDyn     WS Var          | -- "$$a"
                               -- note: "$$a[0]()->a" == "${$a[0]}()->a"
  VarDynExpr WS (WSCap Expr)   -- "${$a . '_'}"
  deriving (Eq, Show, Typeable, Data)

data DynConst = DynConst [(String, WS2)] Var -- "a::$a"
  deriving (Eq, Show, Typeable, Data)

data LRVal =
  LRValVar  DynConst |
  LRValMemb RVal WS2 String -- $a->a
  deriving (Eq, Show, Typeable, Data)

data LOnlyVal =
  LOnlyValList   WS (Either WS [Either WS (WSCap LVal)]) |
  LOnlyValAppend LVal WS2                 | -- "$a[]"
  LOnlyValInd    LOnlyVal WS (WSCap Expr) | -- "$a[][0]"
  LOnlyValMemb   LOnlyVal WS2 String        -- "$a[]->a"
  deriving (Eq, Show, Typeable, Data)

data Const = Const [(String, WS2)] String -- "a::a"
  deriving (Eq, Show, Typeable, Data)

data ROnlyVal =
  ROnlyValConst Const |
  -- "a()", "$a()"
  ROnlyValFunc  (Either LRVal Const) WS (Either WS [WSCap Expr])
  deriving (Eq, Show, Typeable, Data)

lRValOrConstParser = do
  v <- parseW
  case v of
    ValLRVal a -> a
    ValROnlyVal (ROnlyValConst a) -> a
    _ -> fail "Expected LRVal or Const but fould a different Val type."

-- Expr's

data Expr =
  ExprArray     WS (Either WS ([WSCap DubArrowMb], Maybe WS)) |
  ExprAssign    (Maybe WS) LVal WS2 Expr | -- "$res = !$b = $c;"
  ExprBackticks String |
  ExprBinOp     BinOp Expr WS2 Expr |
  -- i'm lazy so just String here instead of like (WS PhpType WS) (fixme?)
  ExprCast      (WSCap String) WS Expr |
  ExprEmpty     WS (WSCap Var) |
  ExprEval      WS (WSCap Expr) |
  ExprExit      Bool (Maybe (WS, Either WS (WSCap Expr))) |
  ExprHereDoc   HereDoc |
  ExprInclude   IncOrReq OnceOrNot WS Expr |
  -- true story: "instanceof" takes LRVal's but not non-Const ROnlyVal's..
  ExprInstOf    Expr WS2 (Either LRVal Const) |
  ExprIsset     WS [WSCap Var] |
  ExprNew       WS (Either DynConst Const)
    (Maybe (WS, Either WS [WSCap Expr])) |
  ExprNumLit    NumLit |
  ExprParen     (WSCap Expr) |
  ExprPreOp     PreOp WS Expr |
  ExprPreIncr   IncrOrDecr WS Var |
  ExprPostIncr  IncrOrDecr Var WS |
  ExprRef       WS Val |
  ExprRVal      RVal |
  ExprStrLit    StrLit |
  ExprTernaryIf TernaryIf
  deriving (Eq, Show, Typeable, Data)

data IncrOrDecr = Incr | Decr
  deriving (Eq, Show, Typeable, Data)

data BinOp = BAnd | BAndWd | BBitAnd | BBitAndBy | BBitOr | BBitOrBy |
  BConcat | BConcatBy | BDiv | BDivBy | BEQ | BGE | BGT | BID | BLE | BLT |
  BMinus | BMinusBy | BMod | BModBy | BMul | BMulBy | BNE |
  -- <> has different precedence than !=
  BNEOld | BNI | BOr | BOrWd | BPlus | BPlusBy | BShiftL | BShiftLBy |
  BShiftR | BShiftRBy | BXor | BXorBy | BXorWd
  deriving (Eq, Show, Typeable, Data)

data PreOp = PrPrint | PrAt | PrBitNot | PrClone | PrNegate | PrNot | PrPos |
  PrSuppress
  deriving (Eq, Show, Typeable, Data)

data IncOrReq = Inc | Req
  deriving (Eq, Show, Typeable, Data)

data OnceOrNot = Once | NotOnce
  deriving (Eq, Show, Typeable, Data)

data TernaryIf = TernaryIf {
  ternaryIfTest :: Expr,
  ternaryIfWS1  :: WS2,
  ternaryIfThen :: Expr,
  ternaryIfWS2  :: WS2,
  ternaryIfElse :: Expr}
  deriving (Eq, Show, Typeable, Data)

data DubArrowMb = DubArrowMb (Maybe (Expr, WS2)) Expr
  deriving (Eq, Show, Typeable, Data)

$(derive makeBinary ''BinOp)
$(derive makeBinary ''Const)
$(derive makeBinary ''DubArrowMb)
$(derive makeBinary ''DynConst)
$(derive makeBinary ''Expr)
$(derive makeBinary ''IncOrReq)
$(derive makeBinary ''IncrOrDecr)
$(derive makeBinary ''LOnlyVal)
$(derive makeBinary ''LRVal)
$(derive makeBinary ''LVal)
$(derive makeBinary ''OnceOrNot)
$(derive makeBinary ''PreOp)
$(derive makeBinary ''ROnlyVal)
$(derive makeBinary ''RVal)
$(derive makeBinary ''TernaryIf)
$(derive makeBinary ''Val)
$(derive makeBinary ''Var)

