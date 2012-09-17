{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Lang.Php.Ast.StmtTypes where

import Text.PrettyPrint.GenericPretty

import qualified Data.Intercal as IC
import qualified Data.List.NonEmpty as NE
import Lang.Php.Ast.ArgList
import Lang.Php.Ast.Common
import Lang.Php.Ast.Lex

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
  deriving (Data, Eq, Generic, Show, Typeable)

data LVal = LValLOnlyVal LOnlyVal | LValLRVal LRVal
  deriving (Data, Eq, Generic, Show, Typeable)

data RVal = RValROnlyVal ROnlyVal | RValLRVal LRVal
  deriving (Data, Eq, Generic, Show, Typeable)

data Var =
  -- In php, indexing is oddly coupled very tightly with being a non-dyn var.
  Var        String [((WS, (Bool, WSCap Expr)))] | -- "$a", "$a[0]", "$a[0][0]"
  VarDyn     WS Var          | -- "$$a"
                               -- note: "$$a[0]()->a" == "${$a[0]}()->a"
  VarDynExpr WS (WSCap Expr)   -- "${$a . '_'}"
  deriving (Data, Eq, Generic, Show, Typeable)

data DynConst = DynConst [(String, WS2)] Var -- "a::$a"
  deriving (Data, Eq, Generic, Show, Typeable)

data LRVal =
  LRValVar     DynConst |
  LRValInd     RVal WS (WSCap Expr) | -- "$a->a[0]"
  LRValMemb    RVal WS2 Memb | -- $a->a
  LRValStaMemb RVal WS2 Memb -- $a::a
  deriving (Data, Eq, Generic, Show, Typeable)

data LOnlyVal =
  LOnlyValList   WS (Either WS [Either WS (WSCap LVal)]) |
  LOnlyValAppend LVal WS2                 | -- "$a[]"
  LOnlyValInd    LOnlyVal WS (WSCap Expr) | -- "$a[][0]"
  LOnlyValMemb   LOnlyVal WS2 Memb          -- "$a[]->a"
  deriving (Data, Eq, Generic, Show, Typeable)

data Const = Const [(String, WS2)] String -- "a::a"
  deriving (Data, Eq, Generic, Show, Typeable)

data ROnlyVal =
  ROnlyValConst Const |
  -- "a()", "$a()"
  ROnlyValFunc  (Either LRVal Const) WS (Either WS [WSCap (Either Expr LVal)])
  deriving (Data, Eq, Generic, Show, Typeable)

data Memb =
  MembStr  String |
  MembVar  Var    |
  MembExpr (WSCap Expr)
  deriving (Data, Eq, Generic, Show, Typeable)

--
-- Expr's
--

data Expr =
  ExprAnonFunc  AnonFunc |
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
  -- FIXME: this fb extension should be separated to a superclass-like Lang?
  ExprIndex     Expr WS (WSCap Expr) |
  ExprInclude   IncOrReq OnceOrNot WS Expr |
  -- true story: "instanceof" takes LRVal's but not non-Const ROnlyVal's..
  ExprInstOf    Expr WS2 (Either LRVal Const) |
  ExprIsset     WS (NE.NonEmpty (WSCap LRVal)) |
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
  deriving (Data, Eq, Generic, Show, Typeable)

data Xml = Xml String
  (IC.Intercal WS (String, Maybe (WS2, Either StrLit (WSCap Expr))))
  (Maybe ([Either XmlLitOrExpr Xml], Bool))
  deriving (Data, Eq, Generic, Show, Typeable)

data XmlLitOrExpr = XmlLit String | XmlExpr (WSCap Expr)
  deriving (Data, Eq, Generic, Show, Typeable)

data BinOp = BAnd | BAndWd | BEQ | BGE | BGT | BID | BLE | BLT | BNE |
  -- <> has different precedence than !=
  BNEOld | BNI | BOr | BOrWd | BXorWd | BByable BinOpBy
  deriving (Data, Eq, Generic, Show, Typeable)

data BinOpBy = BBitAnd | BBitOr | BConcat | BDiv | BMinus | BMod | BMul |
  BPlus | BShiftL | BShiftR | BXor
  deriving (Data, Eq, Generic, Show, Typeable)

data PreOp = PrPrint | PrAt | PrBitNot | PrClone | PrNegate | PrNot | PrPos |
  PrSuppress | PrIncr | PrDecr
  deriving (Data, Eq, Generic, Show, Typeable)

data PostOp = PoIncr | PoDecr
  deriving (Data, Eq, Generic, Show, Typeable)

data IncOrReq = Inc | Req
  deriving (Data, Eq, Generic, Show, Typeable)

data OnceOrNot = Once | NotOnce
  deriving (Data, Eq, Generic, Show, Typeable)

data TernaryIf = TernaryIf {
  ternaryIfCond :: Expr,
  ternaryIfWS1  :: WS2,
  ternaryIfThen :: Maybe Expr,
  ternaryIfWS2  :: WS2,
  ternaryIfElse :: Expr}
  deriving (Data, Eq, Generic, Show, Typeable)

data DubArrowMb = DubArrowMb (Maybe (Expr, WS2)) Expr
  deriving (Data, Eq, Generic, Show, Typeable)

data AnonFuncUse = AnonFuncUse {
  afuncUseArgs :: WSCap (NE.NonEmpty (WSCap FuncArg))}
  deriving (Data, Eq, Generic, Show, Typeable)

data AnonFunc = AnonFunc {
  afuncWS    :: WS,
  afuncRef   :: Maybe WS,
  afuncArgs  :: WSCap (ArgList FuncArg),
  afuncUse   :: Maybe AnonFuncUse,
  afuncBlock :: Block Stmt}
  deriving (Data, Eq, Generic, Show, Typeable)

data FuncArg = FuncArg {
  funcArgType :: Maybe (Maybe Const, WS),
  funcArgRef  :: Maybe WS,
  funcArgVar  :: VarMbVal}
  deriving (Data, Eq, Generic, Show, Typeable)

--
-- Stmt's
--

type StmtList = IC.Intercal WS Stmt

data Stmt =
  StmtBlock     (Block Stmt)                  |
  StmtBreak     (Maybe (WS, Expr)) WS StmtEnd |
  StmtClass     Class                         |
  StmtContinue  (Maybe (WS, Expr)) WS StmtEnd |
  StmtDeclare   Declare |
  StmtDoWhile   DoWhile |
  -- this list must have at least one element.. should i make a type for that?
  StmtEcho      [WSCap Expr] StmtEnd     |
  StmtExpr      Expr WS StmtEnd               |
  StmtFor       For |
  StmtForeach   Foreach |
  StmtFuncDef   Func                          |
  -- this list must have at least one element.. should i make a type for that?
  StmtGlobal    [WSCap Var] StmtEnd           |
  StmtIf        If |
  StmtInterface Interface                     |
  StmtNamespace (WSCap Namespace) StmtEnd     |
  StmtNothing   StmtEnd                       |
  StmtReturn    WS (Maybe (Expr, WS)) StmtEnd |
  -- this list must have at least one element.. should i make a type for that?
  StmtStatic    [WSCap VarMbVal] StmtEnd      |
  StmtSwitch    Switch |
  StmtThrow     (WSCap Expr) StmtEnd          |
  StmtTry       (WSCap (Block Stmt)) (IC.Intercal Catch WS) |
  StmtUnset     (WSCap (NE.NonEmpty (WSCap LRVal))) StmtEnd   |
  StmtUse       (WSCap Use) StmtEnd     |
  StmtWhile     While
  deriving (Data, Eq, Generic, Show, Typeable)

-- a block has {}'s, so one-liner's are not considered blocks
-- and a (Block Stmt) is not the same as a StmtList tho it has the same ast
data Block a = Block (IC.Intercal WS a)
  deriving (Data, Eq, Generic, Show, Typeable)

data Namespace = Namespace String
  deriving (Data, Eq, Generic, Show, Typeable)

data Use = Use String
  deriving (Data, Eq, Generic, Show, Typeable)

data Func = Func {
  funcWS    :: WS,
  funcRef   :: Maybe WS,
  funcName  :: String,
  funcArgs  :: WSCap (Either WS [WSCap FuncArg]),
  funcBlock :: Block Stmt}
  deriving (Data, Eq, Generic, Show, Typeable)

data Interface = Interface {
  ifaceName    :: WSCap Const,
  ifaceExtends :: [WSCap Const],
  ifaceBlock   :: Block IfaceStmt}
  deriving (Data, Eq, Generic, Show, Typeable)

data IfaceStmt =
  IfaceConst [WSCap (VarEqVal Const)] |
  IfaceFunc AbstrFunc
  deriving (Data, Eq, Generic, Show, Typeable)

data AbstrFunc = AbstrFunc {
  abstrFuncPre  :: [(String, WS)],
  abstrFuncRef  :: Maybe WS,
  abstrFuncName :: WSCap Const,
  abstrFuncArgs :: Either WS [WSCap FuncArg],
  abstrFuncWS   :: WS,
  abstrFuncStmtEnd :: StmtEnd}
  deriving (Data, Eq, Generic, Show, Typeable)

data Class = Class {
  classPre     :: [(String, WS)],
  className    :: WSCap String,
  classExtends :: Maybe (WSCap Const),
  classImplements :: [WSCap Const],
  classBlock   :: Block ClassStmt}
  deriving (Data, Eq, Generic, Show, Typeable)

data VarMbVal = VarMbVal Var (Maybe (WS2, Expr))
  deriving (Data, Eq, Generic, Show, Typeable)

data VarEqVal a = VarEqVal a WS2 Expr
  deriving (Data, Eq, Generic, Show, Typeable)

data ClassStmt =
  -- this list must have at least one element.. should i make a type for that?
  CStmtVar (IC.Intercal String WS) [WSCap VarMbVal] StmtEnd |
  CStmtConst [WSCap (VarEqVal Const)] |
  CStmtFuncDef [(String, WS)] Func |
  CStmtAbstrFunc AbstrFunc |
  CStmtCategory String |
  CStmtChildren String |
  CStmtAttribute String
  deriving (Data, Eq, Generic, Show, Typeable)

data DoWhile = DoWhile {
  doWhileBlock   :: WSCap BlockOrStmt,
  doWhileExpr    :: WSCap2 Expr,
  doWhileStmtEnd :: StmtEnd}
  deriving (Data, Eq, Generic, Show, Typeable)

data Declare = Declare {
  declareHeader  :: WSCap (WSCap Const, WSCap Expr),
  declareStmtEnd :: StmtEnd}
  deriving (Data, Eq, Generic, Show, Typeable)

data For = For {
  forHeader :: WSCap (ForPart, ForPart, ForPart),
  forBlock  :: BlockOrStmt}
  deriving (Data, Eq, Generic, Show, Typeable)

data ForPart = ForPart (Either WS [WSCap Expr])
  deriving (Data, Eq, Generic, Show, Typeable)

data Foreach = Foreach {
  foreachHeader :: WSCap (WSCap Expr, WSCap DubArrowMb),
  foreachBlock  :: BlockOrStmt}
  deriving (Data, Eq, Generic, Show, Typeable)

data If = If {
  -- And when ifAltColonSyntax is True, all the BlockOrStmts must be Blocks.
  ifAltColonSyntax :: Bool,
  ifAndIfelses :: IC.Intercal IfBlock (WS, Maybe WS),
  ifElse       :: Maybe (WS2, BlockOrStmt)}
  deriving (Data, Eq, Generic, Show, Typeable)

data IfBlock = IfBlock {
  ifBlockExpr  :: WSCap2 Expr,
  ifBlockBlock :: BlockOrStmt}
  deriving (Data, Eq, Generic, Show, Typeable)

data Switch = Switch {
  switchExpr  :: WSCap2 Expr,
  switchWS    :: WS,
  switchCases :: [Case]}
  deriving (Data, Eq, Generic, Show, Typeable)

data Case = Case {
  caseExpr     :: Either WS (WSCap Expr),
  caseStmtList :: StmtList}
  deriving (Data, Eq, Generic, Show, Typeable)

data Catch = Catch {
  catchHeader :: WSCap (WSCap Const, Expr),
  catchWS     :: WS,
  catchBlock  :: Block Stmt}
  deriving (Data, Eq, Generic, Show, Typeable)

data While = While {
  whileExpr  :: WSCap2 Expr,
  whileBlock :: BlockOrStmt}
  deriving (Data, Eq, Generic, Show, Typeable)

data TopLevel = TopLevel String (Maybe (Either (WSCap Expr, StmtEnd) String))
  deriving (Data, Eq, Generic, Show, Typeable)

data StmtEnd = StmtEndSemi | StmtEndClose TopLevel
  deriving (Data, Eq, Generic, Show, Typeable)

type BlockOrStmt = Either Stmt (Block Stmt)

instance Out AbstrFunc
instance Out AnonFunc
instance Out AnonFuncUse
instance Out BinOp
instance Out BinOpBy
instance (Out a) => Out (Block a)
instance Out Case
instance Out Catch
instance Out Class
instance Out ClassStmt
instance Out Const
instance Out Declare
instance Out DoWhile
instance Out DubArrowMb
instance Out DynConst
instance Out Expr
instance Out For
instance Out Foreach
instance Out ForPart
instance Out Func
instance Out FuncArg
instance Out If
instance Out IfaceStmt
instance Out IfBlock
instance Out IncOrReq
instance Out Interface
instance Out LOnlyVal
instance Out LRVal
instance Out LVal
instance Out Memb
instance Out Namespace
instance Out OnceOrNot
instance Out PostOp
instance Out PreOp
instance Out ROnlyVal
instance Out RVal
instance Out Stmt
instance Out StmtEnd
instance Out Switch
instance Out TernaryIf
instance Out TopLevel
instance Out Use
instance Out Val
instance Out Var
instance (Out a) => Out (VarEqVal a)
instance Out VarMbVal
instance Out While
instance Out Xml
instance Out XmlLitOrExpr
