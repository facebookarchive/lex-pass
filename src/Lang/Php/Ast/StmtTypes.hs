{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Lang.Php.Ast.StmtTypes where

import Text.PrettyPrint.GenericPretty

import Lang.Php.Ast.Common
import Lang.Php.Ast.ExprTypes
import qualified Data.Intercal as IC

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
  StmtUnset     (WSCap [WSCap LRVal]) StmtEnd   |
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

data FuncArg = FuncArg {
  funcArgType :: Maybe (Maybe Const, WS),
  funcArgRef  :: Maybe WS,
  funcArgVar  :: VarMbVal}
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
instance (Out a) => Out (Block a)
instance Out Case
instance Out Catch
instance Out Class
instance Out ClassStmt
instance Out Declare
instance Out DoWhile
instance Out For
instance Out ForPart
instance Out Foreach
instance Out Func
instance Out FuncArg
instance Out If
instance Out IfaceStmt
instance Out IfBlock
instance Out Interface
instance Out Namespace
instance Out Stmt
instance Out StmtEnd
instance Out Switch
instance Out TopLevel
instance Out Use
instance Out VarMbVal
instance (Out a) => Out (VarEqVal a)
instance Out While

$(derive makeBinary ''AbstrFunc)
$(derive makeBinary ''Block)
$(derive makeBinary ''Case)
$(derive makeBinary ''Catch)
$(derive makeBinary ''Class)
$(derive makeBinary ''ClassStmt)
$(derive makeBinary ''Declare)
$(derive makeBinary ''DoWhile)
$(derive makeBinary ''For)
$(derive makeBinary ''ForPart)
$(derive makeBinary ''Foreach)
$(derive makeBinary ''Func)
$(derive makeBinary ''FuncArg)
$(derive makeBinary ''If)
$(derive makeBinary ''IfaceStmt)
$(derive makeBinary ''IfBlock)
$(derive makeBinary ''Interface)
$(derive makeBinary ''Namespace)
$(derive makeBinary ''Stmt)
$(derive makeBinary ''StmtEnd)
$(derive makeBinary ''Switch)
$(derive makeBinary ''TopLevel)
$(derive makeBinary ''Use)
$(derive makeBinary ''VarMbVal)
$(derive makeBinary ''VarEqVal)
$(derive makeBinary ''While)

