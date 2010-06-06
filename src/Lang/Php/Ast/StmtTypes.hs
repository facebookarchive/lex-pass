{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.Php.Ast.StmtTypes where

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
  StmtNothing   StmtEnd                       |
  StmtReturn    WS (Maybe (Expr, WS)) StmtEnd |
  -- this list must have at least one element.. should i make a type for that?
  StmtStatic    [WSCap VarMbVal] StmtEnd      |
  StmtSwitch    Switch |
  StmtThrow     (WSCap Expr) StmtEnd          |
  StmtTry       (WSCap (Block Stmt)) (IC.Intercal Catch WS) |
  StmtUnset     (WSCap [WSCap LRVal]) StmtEnd   |
  StmtWhile     While
  deriving (Eq, Show, Typeable, Data)

-- a block has {}'s, so one-liner's are not considered blocks
-- and a (Block Stmt) is not the same as a StmtList tho it has the same ast
data Block a = Block (IC.Intercal WS a)
  deriving (Eq, Show, Typeable, Data)

data Func = Func {
  funcWS    :: WS,
  funcRef   :: Maybe WS,
  funcName  :: String,
  funcArgs  :: WSCap (Either WS [WSCap FuncArg]),
  funcBlock :: Block Stmt}
  deriving (Eq, Show, Typeable, Data)

data Interface = Interface {
  ifaceName    :: WSCap Const,
  ifaceExtends :: [WSCap Const],
  ifaceBlock   :: Block IfaceStmt}
  deriving (Eq, Show, Typeable, Data)

data IfaceStmt =
  IfaceConst [WSCap (VarEqVal Const)] |
  IfaceFunc AbstrFunc
  deriving (Eq, Show, Typeable, Data)

data AbstrFunc = AbstrFunc {
  abstrFuncPre  :: [(String, WS)],
  abstrFuncRef  :: Maybe WS,
  abstrFuncName :: WSCap Const,
  abstrFuncArgs :: Either WS [WSCap FuncArg],
  abstrFuncWS   :: WS,
  abstrFuncStmtEnd :: StmtEnd}
  deriving (Eq, Show, Typeable, Data)

data Class = Class {
  classPre     :: [(String, WS)],
  className    :: WSCap String,
  classExtends :: Maybe (WSCap Const),
  classImplements :: [WSCap Const],
  classBlock   :: Block ClassStmt}
  deriving (Eq, Show, Typeable, Data)

data FuncArg = FuncArg {
  funcArgType :: Maybe (Maybe Const, WS),
  funcArgRef  :: Maybe WS,
  funcArgVar  :: VarMbVal}
  deriving (Eq, Show, Typeable, Data)

data VarMbVal = VarMbVal Var (Maybe (WS2, Expr))
  deriving (Eq, Show, Typeable, Data)

data VarEqVal a = VarEqVal a WS2 Expr
  deriving (Eq, Show, Typeable, Data)

data ClassStmt =
  -- this list must have at least one element.. should i make a type for that?
  CStmtVar (IC.Intercal String WS) [WSCap VarMbVal] StmtEnd |
  CStmtConst [WSCap (VarEqVal Const)] |
  CStmtFuncDef [(String, WS)] Func |
  CStmtAbstrFunc AbstrFunc |
  CStmtCategory String |
  CStmtChildren String |
  CStmtAttribute String
  deriving (Eq, Show, Typeable, Data)

data DoWhile = DoWhile {
  doWhileBlock   :: WSCap BlockOrStmt,
  doWhileExpr    :: WSCap2 Expr,
  doWhileStmtEnd :: StmtEnd}
  deriving (Eq, Show, Typeable, Data)

data Declare = Declare {
  declareHeader  :: WSCap (WSCap Const, WSCap Expr),
  declareStmtEnd :: StmtEnd}
  deriving (Eq, Show, Typeable, Data)

data For = For {
  forHeader :: WSCap (ForPart, ForPart, ForPart),
  forBlock  :: BlockOrStmt}
  deriving (Eq, Show, Typeable, Data)

data ForPart = ForPart (Either WS [WSCap Expr])
  deriving (Eq, Show, Typeable, Data)

data Foreach = Foreach {
  foreachHeader :: WSCap (WSCap Expr, WSCap DubArrowMb),
  foreachBlock  :: BlockOrStmt}
  deriving (Eq, Show, Typeable, Data)

data If = If {
  ifAndIfelses :: IC.Intercal IfBlock (WS, Maybe WS),
  ifElse       :: Maybe (WS2, BlockOrStmt)}
  deriving (Eq, Show, Typeable, Data)

data IfBlock = IfBlock {
  ifBlockExpr  :: WSCap2 Expr,
  ifBlockBlock :: BlockOrStmt}
  deriving (Eq, Show, Typeable, Data)

data Switch = Switch {
  switchExpr  :: WSCap2 Expr,
  switchWS    :: WS,
  switchCases :: [Case]}
  deriving (Eq, Show, Typeable, Data)

data Case = Case {
  caseExpr     :: Either WS (WSCap Expr),
  caseStmtList :: StmtList}
  deriving (Eq, Show, Typeable, Data)

data Catch = Catch {
  catchHeader :: WSCap (WSCap Const, Expr),
  catchWS     :: WS,
  catchBlock  :: Block Stmt}
  deriving (Eq, Show, Typeable, Data)

data While = While {
  whileExpr  :: WSCap2 Expr,
  whileBlock :: BlockOrStmt}
  deriving (Eq, Show, Typeable, Data)

data TopLevel = TopLevel String (Maybe (Either (WSCap Expr, StmtEnd) String))
  deriving (Eq, Show, Typeable, Data)

data StmtEnd = StmtEndSemi | StmtEndClose TopLevel
  deriving (Eq, Show, Typeable, Data)

type BlockOrStmt = Either Stmt (Block Stmt)

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
$(derive makeBinary ''Stmt)
$(derive makeBinary ''StmtEnd)
$(derive makeBinary ''Switch)
$(derive makeBinary ''TopLevel)
$(derive makeBinary ''VarMbVal)
$(derive makeBinary ''VarEqVal)
$(derive makeBinary ''While)

