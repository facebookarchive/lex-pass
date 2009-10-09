{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.Php.Ast.StmtTypes where

import Lang.Php.Ast.Common
import Lang.Php.Ast.ExprTypes
import qualified Data.Intercal as IC

type StmtList = InterWS Stmt

data Stmt =
  StmtBlock     (Block Stmt)                  |
  StmtBreak     (Maybe (WS, Expr)) WS StmtEnd |
  StmtClass     Class                         |
  StmtContinue  (Maybe (WS, Expr)) WS StmtEnd |
  StmtDeclare   Declare |
  StmtDoWhile   DoWhile |
  -- this list must have at least one element.. should i make a type for that?
  StmtEcho      Bool [WSCap Expr] StmtEnd     |
  StmtExpr      Expr WS StmtEnd               |
  StmtFor       For |
  StmtForeach   Foreach |
  StmtFuncDef   Func                          |
  -- this list must have at least one element.. should i make a type for that?
  StmtGlobal    [WSCap Var] StmtEnd           |
  StmtIf        If |
  StmtInterface Interface                     |
  StmtNothing   StmtEnd                       |
  StmtReturn    (Maybe (WS, Expr)) WS StmtEnd |
  -- this list must have at least one element.. should i make a type for that?
  StmtStatic    [WSCap VarMbVal] StmtEnd      |
  StmtSwitch    Switch |
  StmtThrow     WS Expr WS StmtEnd            |
  -- this list must have at least one element.. should i make a type for that?
  StmtTry       WS (Block Stmt) [Catch] |
  StmtUnset     WS [WSCap Var] WS StmtEnd     |
  StmtWhile     While
  deriving (Eq, Show, Typeable, Data)

-- a block has {}'s, so one-liner's are not considered blocks
-- and a (Block Stmt) is not the same as a StmtList tho it has the same ast
data Block a = Block (IC.Intercal WS a)
  deriving (Eq, Show, Typeable, Data)

data Func = Func {
  funcWS1   :: WS,
  funcRef   :: Maybe WS,
  funcName  :: String,
  funcWS2   :: WS,
  funcArgs  :: Either WS [FuncArg],
  funcWS3   :: WS,
  funcBlock :: Block Stmt
  }
  deriving (Eq, Show, Typeable, Data)

data Interface = Interface {
  ifaceWS1     :: WS,
  ifaceName    :: Const,
  ifaceWS2     :: WS,
  ifaceExtends :: [WSCap Const],
  ifaceBlock   :: Block IfaceStmt
  }
  deriving (Eq, Show, Typeable, Data)

data IfaceStmt =
  IfaceConst [WSCap (VarVal Const)] |
  IfaceFunc [(String, WS)] WS Const WS (Either WS [FuncArg]) WS StmtEnd
  deriving (Eq, Show, Typeable, Data)

data Class = Class {
  classPre     :: [(String, WS)],
  classWS1     :: WS,
  className    :: String,
  classExtends :: Maybe (WS2, Const),
  classWS2     :: WS,
  classImplements :: [WSCap Const],
  classBlock   :: Block ClassStmt
  }
  deriving (Eq, Show, Typeable, Data)

-- named records here?
data FuncArg =
  FuncArg WS (Maybe (Maybe Const, WS)) (Maybe WS) Var (Maybe (WS2, Expr)) WS
  deriving (Eq, Show, Typeable, Data)

data VarMbVal = VarMbVal Var (Maybe (WS2, Expr))
  deriving (Eq, Show, Typeable, Data)

data VarVal a = VarVal a WS2 Expr
  deriving (Eq, Show, Typeable, Data)

data ClassStmt =
  CStmtVar (IC.Intercal String WS) [WSCap VarMbVal] StmtEnd |
  CStmtConst [WSCap (VarVal Const)] |
  CStmtFuncDef [(String, WS)] Func |
  CStmtAbsFunc [(String, WS)] WS Const WS (Either WS [FuncArg]) WS StmtEnd
  deriving (Eq, Show, Typeable, Data)

data DoWhile = DoWhile {
  doWhileBlock   :: WSCap BlockOrStmt,
  doWhileWS1     :: WS2,
  doWhileExpr    :: Expr,
  doWhileWS2     :: WS2,
  doWhileStmtEnd :: StmtEnd
  }
  deriving (Eq, Show, Typeable, Data)

data Declare = Declare {
  declareWS1     :: WS2,
  declareConst   :: Const,
  declareWS2     :: WS2,
  declareExpr    :: Expr,
  declareWS3     :: WS2,
  declareStmtEnd :: StmtEnd
  }
  deriving (Eq, Show, Typeable, Data)

data For = For {
  forWS1   :: WS,
  forInit  :: ForPart,
  forTest  :: ForPart,
  forStep  :: ForPart,
  forWS2   :: WS,
  forBlock :: BlockOrStmt
  }
  deriving (Eq, Show, Typeable, Data)

data Foreach = Foreach {
  foreachWS1        :: WS2,
  foreachExpr       :: Expr,
  foreachWS2        :: WS2,
  foreachDubArrowMb :: DubArrowMb,
  foreachWS3        :: WS2,
  foreachBlock      :: BlockOrStmt
  }
  deriving (Eq, Show, Typeable, Data)

data If = If {
  ifAndIfelses :: IC.Intercal IfBlock (WS, Maybe WS),
  ifElse       :: Maybe (WS2, BlockOrStmt)
  }
  deriving (Eq, Show, Typeable, Data)

data IfBlock = IfBlock {
  ifBlockWS1   :: WS2,
  ifBlockExpr  :: Expr,
  ifBlockWS2   :: WS2,
  ifBlockBlock :: BlockOrStmt
  }
  deriving (Eq, Show, Typeable, Data)

data Switch = Switch {
  switchWS1   :: WS,
  switchExpr  :: WSCap Expr,
  switchWS2   :: WS2,
  switchCases :: [Case]
  }
  deriving (Eq, Show, Typeable, Data)

data Case = Case {
  caseMbWSExpr :: Maybe (WS, Expr),
  caseWS1      :: WS,
  caseStmtList :: StmtList
  }
  deriving (Eq, Show, Typeable, Data)

data Catch = Catch {
  catchWS1   :: WS2,
  catchConst :: WSCap Const,
  catchExpr  :: Expr,
  catchWS2   :: WS2,
  catchBlock :: Block Stmt
  }
  deriving (Eq, Show, Typeable, Data)

data While = While {
  whileWS1   :: WS2,
  whileExpr  :: Expr,
  whileWS2   :: WS2,
  whileBlock :: BlockOrStmt
  }
  deriving (Eq, Show, Typeable, Data)

type ForPart = Either WS [WSCap Expr]

-- statements can end in ";" or not (and just be followed by a "?>")
-- php also oddly distinguishes "?>" and "?>\n", with no other possibilities.
-- (note this differs from e.g. "<?=\n" which is treated as "<?=" + "\n".)
data StmtEnd = StmtEndSemi | StmtEndClose | StmtEndCloseNL
  deriving (Eq, Show, Typeable, Data)

type BlockOrStmt = Either Stmt (Block Stmt)

$(derive makeBinary ''Block)
$(derive makeBinary ''Case)
$(derive makeBinary ''Catch)
$(derive makeBinary ''Class)
$(derive makeBinary ''ClassStmt)
$(derive makeBinary ''Declare)
$(derive makeBinary ''DoWhile)
$(derive makeBinary ''For)
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
$(derive makeBinary ''VarMbVal)
$(derive makeBinary ''VarVal)
$(derive makeBinary ''While)

