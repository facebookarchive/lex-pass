{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, TypeSynonymInstances #-}

module Lang.Php.Ast where

import Control.Applicative ((<$>), (*>), (<*))
import Control.Arrow
import Control.Monad
import Control.Monad.Identity
import Data.Ast
import Data.Binary
import Data.DeriveTH
import Data.Generics hiding (Prefix, Infix)
import Data.List
import Data.Maybe
import Data.Tok
import Data.Typeable
import FUtil
import Lang.Php.Tok
import Text.Parsec hiding (satisfy, oneOf, noneOf, anyToken)
import Text.Parsec.Expr
import Text.Parsec.Pos
import qualified Data.Intercal as IC

-- abstract syntax tree for a php file

-- we cheat in a few ways for simplicity since we don't care and rigidity can
-- always be added later.

-- we don't include delimiters in the ast.  so Var has no $ etc
-- however, currently string literals are just as such
--    e.g. "'i\\'m'" or "\"i'm\"" for the same php string for: i'm

-- certain legal but stupid things are not treated here and will not parse,
-- such as the third param of define()

-- i'm just counting html and <?php etc tokens as WS at least for now
-- since they can appear like anywhere in the same way.
-- comments count as WS as well.

-- note that php has an exception regarding ! where e.g.
-- "$a = ($b = !!!$c .= $d = false) + 10;"
-- is allowed.  we don't treat this deeply currently (ever?) but merely let
-- ExprAssign etc take an Expr for the first arg instead of a Var.

-- TODO: need to split up concept of var (can appear in e.g. global ..) which
-- is subset of lval (can be assigned to)
-- TODO: also might need to do a bit with optimizing around var/lval.. things
-- got much slower with them

-- move this..
absorbWs :: [Tok] -> InterWS Tok
absorbWs toks = if null rest
  then IC.Interend ws
  else IC.Intercal ws rest1 $ absorbWs restRest
  where
  (ws, rest) = span ((`elem` wsTokTypes) . tokGetType) toks
  rest1:restRest = rest

type StmtList = InterWS Stmt

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

data IfaceStmt =
  IfaceConst [WSCap (VarVal Const)] |
  IfaceFunc [(Tok, WS)] WS Const WS (Either WS [FuncArg]) WS StmtEnd
  deriving (Eq, Show, Typeable, Data)

data ClassStmt =
  CStmtVar (IC.Intercal Tok WS) [WSCap VarMbVal] StmtEnd |
  CStmtConst [WSCap (VarVal Const)] |
  CStmtFuncDef [(Tok, WS)] Func |
  CStmtAbsFunc [(Tok, WS)] WS Const WS (Either WS [FuncArg]) WS StmtEnd
  deriving (Eq, Show, Typeable, Data)

data VarMbVal = VarMbVal Var (Maybe (WS, WS, Expr))
  deriving (Eq, Show, Typeable, Data)

data VarVal a = VarVal a WS WS Expr
  deriving (Eq, Show, Typeable, Data)

data DoWhile = DoWhile {
  doWhileWS1     :: WS,
  doWhileBlock   :: BlockOrStmt,
  doWhileWS2     :: WS,
  doWhileWS3     :: WS,
  doWhileWS4     :: WS,
  doWhileExpr    :: Expr,
  doWhileWS5     :: WS,
  doWhileWS6     :: WS,
  doWhileStmtEnd :: StmtEnd
  }
  deriving (Eq, Show, Typeable, Data)

data Declare = Declare {
  declareWS1     :: WS,
  declareWS2     :: WS,
  declareConst   :: Const,
  declareWS3     :: WS,
  declareWS4     :: WS,
  declareExpr    :: Expr,
  declareWS5     :: WS,
  declareWS6     :: WS,
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
  foreachWS1        :: WS,
  foreachWS2        :: WS,
  foreachExpr       :: Expr,
  foreachWS3        :: WS,
  foreachWS4        :: WS,
  foreachDubArrowMb :: DubArrowMb,
  foreachWS5        :: WS,
  foreachWS6        :: WS,
  foreachBlock      :: BlockOrStmt
  }
  deriving (Eq, Show, Typeable, Data)

data If = If {
  ifAndIfelses :: IC.Intercal IfBlock (WS, Maybe WS),
  ifElse       :: Maybe (WS, WS, BlockOrStmt)
  }
  deriving (Eq, Show, Typeable, Data)

data IfBlock = IfBlock {
  ifBlockWS1   :: WS,
  ifBlockWS2   :: WS,
  ifBlockExpr  :: Expr,
  ifBlockWS3   :: WS,
  ifBlockWS4   :: WS,
  ifBlockBlock :: BlockOrStmt
  }
  deriving (Eq, Show, Typeable, Data)

data Switch = Switch {
  switchWS1   :: WS,
  switchWS2   :: WS,
  switchExpr  :: Expr,
  switchWS3   :: WS,
  switchWS4   :: WS,
  switchWS5   :: WS,
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
  catchWS1   :: WS,
  catchWS2   :: WS,
  catchWS3   :: WS,
  catchConst :: Const,
  catchWS4   :: WS,
  catchExpr  :: Expr,
  catchWS5   :: WS,
  catchWS6   :: WS,
  catchBlock :: Block Stmt
  }
  deriving (Eq, Show, Typeable, Data)

data While = While {
  whileWS1   :: WS,
  whileWS2   :: WS,
  whileExpr  :: Expr,
  whileWS3   :: WS,
  whileWS4   :: WS,
  whileBlock :: BlockOrStmt
  }
  deriving (Eq, Show, Typeable, Data)

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

type ForPart = Either WS [WSCap Expr]

-- statements can end in ";" or not (and just be followed by a "?>")
-- php also oddly distinguishes "?>" and "?>\n", with no other possibilities.
-- (note this differs from e.g. "<?=\n" which is treated as "<?=" + "\n".)
data StmtEnd = StmtEndSemi | StmtEndClose | StmtEndCloseNL
  deriving (Eq, Show, Typeable, Data)

data Define = Define {
  defineWS1   :: WS,
  defineWS2   :: WS,
  defineLabel :: SV,
  defineWS3   :: WS,
  defineWS4   :: WS,
  defineValue :: Expr,
  defineWS5   :: WS
  }
  deriving (Eq, Show, Typeable, Data)

data List = List {
  listWS1  :: WS,
  -- "list( ) = array();" is legit php
  listVars :: [Either WS (WSCap Var)],
  listWS2  :: WS,
  listWS3  :: WS,
  listExpr :: Expr
  }
  deriving (Eq, Show, Typeable, Data)

data TernaryIf = TernaryIf {
  ternaryIfTest :: Expr,
  ternaryIfWS1  :: WS,
  ternaryIfWS2  :: WS,
  ternaryIfThen :: Expr,
  ternaryIfWS3  :: WS,
  ternaryIfWS4  :: WS,
  ternaryIfElse :: Expr
  }
  deriving (Eq, Show, Typeable, Data)

data Expr =
  -- you would have thought these were statements

  ExprAssign    Expr WS WS Expr               |
  ExprBitAndBy  Expr WS WS Expr               |
  ExprBitOrBy   Expr WS WS Expr               |
  ExprBitXorBy  Expr WS WS Expr               |
  ExprConcatBy  Expr WS WS Expr               |
  ExprDefine    Define |
  ExprDivBy     Expr WS WS Expr               |
  ExprExit      Bool (Maybe (WS, Either WS (WSCap Expr))) |
  ExprExtract   WS WS Expr WS                 |
  ExprInclude   IncOrReq OnceOrNot WS Expr    |
  ExprList      List |
  ExprMinusBy   Expr WS WS Expr               |
  ExprModBy     Expr WS WS Expr               |
  ExprMulBy     Expr WS WS Expr               |
  ExprPlusBy    Expr WS WS Expr               |
  ExprPrint     WS Expr                       |
  ExprShiftLBy  Expr WS WS Expr               |
  ExprShiftRBy  Expr WS WS Expr               |
  ExprTernaryIf TernaryIf |

  -- legit expressions

  ExprAnd      Expr WS WS Expr                |
  ExprAndWd    Expr WS WS Expr                |
  ExprArray    WS (Either WS ([WSCap DubArrowMb], Maybe WS)) |
  ExprAt       WS Expr                        |
  ExprBackticks (IC.Intercal WS Tok)          |
  ExprBitAnd   Expr WS WS Expr                |
  ExprBitNot   WS Expr                        |
  ExprBitOr    Expr WS WS Expr                |
  ExprBitXor   Expr WS WS Expr                |
  -- i'm lazy so just (Tok) here instead of like (WS PhpType WS)
  ExprCast     Tok WS Expr                    |
  ExprClone    WS Expr                        |
  ExprConcat   Expr WS WS Expr                |
  ExprConst    Const                          |
  ExprDiv      Expr WS WS Expr                |
  ExprEmpty    WS WS Var WS                   |
  ExprEQ       Expr WS WS Expr                |
  ExprEval     WS WS Expr WS                  |
  -- loosening bc it's actually harder to make a parser with php's arbitrary
  --    restrictions
  --ExprFuncCall SV WS [WSCap Expr]        |
  ExprFuncCall Expr WS (Either WS [WSCap Expr]) |
  ExprGE       Expr WS WS Expr                |
  ExprGT       Expr WS WS Expr                |
  ExprID       Expr WS WS Expr                |
  ExprIndex    Expr WS WS Expr WS             |
  ExprInstOf   Expr WS WS SV                  |
  ExprIsset    WS [WSCap Var]                 |
  ExprLE       Expr WS WS Expr                |
  ExprLT       Expr WS WS Expr                |
  -- method calls look odd this way? separate them?
  -- loosening bc it's actually harder to make a parser with php's arbitrary
  --    restrictions
  --ExprMember   Var WS WS SV                   |
  ExprMember   Expr WS WS Expr                |
  ExprMinus    Expr WS WS Expr                |
  ExprMod      Expr WS WS Expr                |
  ExprMul      Expr WS WS Expr                |
  ExprNegate   WS Expr                        |
  ExprNew      WS SV
    (Maybe (WS, Either WS [WSCap Expr]))      |
  ExprNE       Expr WS WS Expr                |
  -- <> has different precedence than !=
  ExprNEOld    Expr WS WS Expr                |
  ExprNI       Expr WS WS Expr                |
  ExprNot      WS Expr                        |
  -- should we actually read the int/double's?  seems dangerous with
  -- floating point wackiness and php's large-ints-become-floats
  ExprNum      String                         |
  ExprOr       Expr WS WS Expr                |
  ExprOrWd     Expr WS WS Expr                |
  ExprParen    WS Expr WS                     |
  ExprPlus     Expr WS WS Expr                |
  ExprPos      WS Expr                        |
  ExprPostDecr Expr WS                        |
  ExprPostIncr Expr WS                        |
  ExprPreDecr  WS Expr                        |
  ExprPreIncr  WS Expr                        |
  ExprRef      WS Expr                        |
  -- loosening bc it's actually harder to make a parser with php's arbitrary
  --    restrictions
  --ExprStatMemb SV WS WS SV                    |
  ExprStatMemb Expr WS WS Expr                |
  ExprStrLit   StrLit                         |
  -- L = Left = get bigger (mnemonic: problems left alone get bigger)
  ExprShiftL   Expr WS WS Expr                |
  ExprShiftR   Expr WS WS Expr                |
  ExprSuppress WS Expr                        |
  ExprVar      Var                            |
  ExprXor      Expr WS WS Expr                |
  ExprXorWd    Expr WS WS Expr
  deriving (Eq, Show, Typeable, Data)

data DubArrowMb = DubArrowMb (Maybe (Expr, WS, WS)) Expr
  deriving (Eq, Show, Typeable, Data)

-- var means lval
data Var =
  Var String |
  DynVar WS Var |
  DynVarExpr WS WS Expr WS |
  VarIndex SV WS (Maybe (WS, Expr)) WS |
  VarMember Var WS WS (Either (WSCap Expr) SV) |
  VarStaticMember Const WS WS Var |
  VarFuncCall SV WS (Either WS [WSCap Expr]) |
  VarRef WS Var
  deriving (Eq, Show, Typeable, Data)

type SV = Either Var Const

data Interface = Interface {
  ifaceWS1     :: WS,
  ifaceName    :: Const,
  ifaceWS2     :: WS,
  ifaceExtends :: [WSCap Const],
  ifaceBlock   :: Block IfaceStmt
  }
  deriving (Eq, Show, Typeable, Data)

-- a block has {}'s, so one-liner's are not considered blocks
-- and a (Block Stmt) is not the same as a StmtList tho it has the same ast
data Block a = Block (IC.Intercal WS a)
  deriving (Eq, Show, Typeable, Data)

type BlockOrStmt = Either Stmt (Block Stmt)

data Class = Class {
  classPre     :: [(Tok, WS)],
  classWS1     :: WS,
  className    :: String,
  classExtends :: Maybe (WS, WS, Const),
  classWS2     :: WS,
  classImplements :: [WSCap Const],
  classBlock   :: Block ClassStmt
  }
  deriving (Eq, Show, Typeable, Data)

data IncOrReq = Inc | Req
  deriving (Eq, Show, Typeable, Data)
data OnceOrNot = Once | NotOnce
  deriving (Eq, Show, Typeable, Data)

-- named records here?
data FuncArg =
  FuncArg WS (Maybe (Maybe Const, WS)) (Maybe WS) Var (Maybe (WS, WS, Expr)) WS
  deriving (Eq, Show, Typeable, Data)

-- hm is Const overused right now?
data Const = Const String |
  ClassConst String WS WS Const
  deriving (Eq, Show, Typeable, Data)

data StrLit = StrLit String | StrExpr [TokWS] WS | StrHereDoc String [TokWS] WS
  deriving (Eq, Show, Typeable, Data)

instance ToToks IfaceStmt where
  toToks (IfaceConst vars) = cStmtConstToToks vars
  toToks (IfaceFunc pre ws1 name ws2 args ws3 stmtEnd) =
    cStmtAbsFuncToToks pre ws1 name ws2 args ws3 stmtEnd

instance ToToks ClassStmt where
  toToks (CStmtVar pre varMbVals stmtEnd) = concat [toToks pre,
    intercalate [tokComma] $ map toToks varMbVals, toToks stmtEnd]
  toToks (CStmtConst vars) = cStmtConstToToks vars
  toToks (CStmtFuncDef pre func) = toToks pre ++ toToks func
  toToks (CStmtAbsFunc pre ws1 name ws2 args ws3 stmtEnd) =
    cStmtAbsFuncToToks pre ws1 name ws2 args ws3 stmtEnd

instance ToToks Declare where
  toToks (Declare ws1 ws2 name ws3 ws4 expr ws5 ws6 stmtEnd) =
    concat [[tokDeclare], ws1, [tokLParen], ws2, toToks name, ws3, [tokEquals],
    ws4, toToks expr, ws5, [tokRParen], ws6, toToks stmtEnd]

instance ToToks DoWhile where
  toToks (DoWhile ws1 block ws2 ws3 ws4 expr ws5 ws6 stmtEnd) =
    concat [[tokDo], ws1, toToks block, ws2, [tokWhile], ws3, [tokLParen],
    ws4, toToks expr, ws5, [tokRParen], ws6, toToks stmtEnd]

instance ToToks For where
  toToks (For ws1 inits conds incrs ws2 block) = concat [
    [tokFor], ws1, [tokLParen],
    intercalate [tokSemi] $ map csvToToks [inits, conds, incrs], [tokRParen],
    ws2, toToks block]
    where
    csvToToks = either id $ intercalate [tokComma] . map toToks

instance ToToks Foreach where
  toToks (Foreach ws1 ws2 expr ws3 ws4 dubArrow ws5 ws6 block) = concat [
    [tokForeach], ws1, [tokLParen], ws2, toToks expr, ws3, [tokAs], ws4,
    toToks dubArrow, ws5, [tokRParen], ws6, toToks block]

instance ToToks IfBlock where
  toToks (IfBlock ws1 ws2 expr ws3 ws4 block) =
    concat [ws1, [tokLParen], ws2, toToks expr, ws3,
    [tokRParen], ws4, toToks block]

instance ToToks If where
  toToks (If ifAndIfelses theElse) =
    [tokIf] ++ toToks theIf ++
    concatMap doIfelse ifelses ++
    maybe [] (\ (ws1, ws2, block) -> ws1 ++ [tokElse] ++ ws2 ++ toToks block)
      theElse
    where
    (theIf, ifelses) = IC.breakStart ifAndIfelses
    doElsery Nothing = [tokElseif]
    doElsery (Just ws) = [tokElse] ++ ws ++ [tokIf]
    doIfelse ((ws, elsery), condAndBlock) =
      ws ++ doElsery elsery ++ toToks condAndBlock

instance ToToks Case where
  toToks (Case header wsC stmtList) =
    maybe [tokDefault] (\ (ws, expr) -> [tokCase] ++ ws ++ toToks expr) header
    ++ wsC ++ [tokColon] ++ toToks stmtList

instance ToToks Switch where
  toToks (Switch ws1 ws2 expr ws3 ws4 ws5 cases) = concat [
    [tokSwitch], ws1, [tokLParen], ws2, toToks expr, ws3, [tokRParen], ws4,
    [tokLBrace], ws5, toToks cases, [tokRBrace]]

instance ToToks Catch where
  toToks (Catch ws1 ws2 ws3 const ws4 expr ws5 ws6 block) =
    ws1 ++ [tokCatch] ++ ws2 ++ [tokLParen] ++ ws3 ++ toToks const ++
    ws4 ++ toToks expr ++ ws5 ++ [tokRParen] ++
    ws6 ++ toToks block

instance ToToks While where
  toToks (While ws1 ws2 expr ws3 ws4 block) =
    concat [[tokWhile], ws1, [tokLParen], ws2, toToks expr, ws3,
      [tokRParen], ws4, toToks block]

instance ToToks Stmt where
  toToks (StmtBlock a) = toToks a
  toToks (StmtBreak a b stmtEnd) =
    concat [[tokBreak], toToks a, toToks b, toToks stmtEnd]
  toToks (StmtClass a) = toToks a
  toToks (StmtContinue a b stmtEnd) =
    concat [[tokContinue], toToks a, toToks b, toToks stmtEnd]
  toToks (StmtDeclare a) = toToks a
  toToks (StmtDoWhile a) = toToks a
  toToks (StmtEcho isEcho a stmtEnd) = concat [
    [if isEcho then tokEcho else tokOpenTagWithEcho],
    intercalate [tokComma] $ map toToks a, toToks stmtEnd]
  toToks (StmtExpr a b stmtEnd) = concat [toToks a, toToks b, toToks stmtEnd]
  toToks (StmtFor a) = toToks a
  toToks (StmtForeach a) = toToks a
  toToks (StmtFuncDef a) = toToks a
  toToks (StmtGlobal [] _) =
    error "global list must have at least one element."
  toToks (StmtGlobal a stmtEnd) = concat [[tokGlobal],
    intercalate [tokComma] $ map toToks a, toToks stmtEnd]
  toToks (StmtIf a) = toToks a
  toToks (StmtInterface a) = toToks a
  toToks (StmtNothing stmtEnd) = toToks stmtEnd
  toToks (StmtReturn a b stmtEnd) =
    concat [[tokReturn], toToks a, toToks b, toToks stmtEnd]
  toToks (StmtSwitch a) = toToks a
  toToks (StmtThrow ws1 expr ws2 stmtEnd) =
    concat [[tokThrow], ws1, toToks expr, ws2, toToks stmtEnd]
  toToks (StmtTry ws block catches) =
    concat [[tokTry], ws, toToks block, toToks catches]
  toToks (StmtUnset ws1 vds ws2 stmtEnd) =
    concat [[tokUnset], ws1, [tokLParen],
    intercalate [tokComma] $ map toToks vds, [tokRParen], ws2, toToks stmtEnd]
  toToks (StmtStatic [] _) =
    error "static list must have at least one element."
  toToks (StmtStatic a stmtEnd) = concat [[tokStatic],
    intercalate [tokComma] $ map toToks a, toToks stmtEnd]
  toToks (StmtWhile a) = toToks a

instance ToToks Define where
  toToks (Define ws1 ws2 svd ws3 ws4 expr ws5) = concat [[tokDefine],
    ws1, [tokLParen], ws2, toToks svd, ws3, [tokComma], ws4, toToks expr,
    ws5, [tokRParen]]

instance ToToks List where
  toToks (List ws1 vars ws2 ws3 expr) = concat [[tokList], ws1,
    [tokLParen], intercalate [tokComma] $ map toToks vars,
    [tokRParen], ws2, [tokEquals], ws3, toToks expr]

instance ToToks TernaryIf where
  toToks (TernaryIf expr1 ws1 ws2 expr2 ws3 ws4 expr3) =
    concat [toToks expr1, ws1, [tokQuestion], ws2, toToks expr2, ws3,
    [tokColon], ws4, toToks expr3]

instance ToToks Expr where
  toToks (ExprAssign var ws1 ws2 expr) =
    exprToToks var ws1 ws2 expr tokEquals
  toToks (ExprBitAndBy var ws1 ws2 expr) =
    exprToToks var ws1 ws2 expr tokBitAndBy
  toToks (ExprBitOrBy var ws1 ws2 expr) =
    exprToToks var ws1 ws2 expr tokBitOrBy
  toToks (ExprBitXorBy var ws1 ws2 expr) =
    exprToToks var ws1 ws2 expr tokBitXorBy
  toToks (ExprConcatBy var ws1 ws2 expr) =
    exprToToks var ws1 ws2 expr tokConcatBy
  toToks (ExprDefine a) = toToks a
  toToks (ExprDivBy var ws1 ws2 expr) =
    exprToToks var ws1 ws2 expr tokDivBy
  toToks (ExprExit isExit a) =
    concat [[if isExit then tokExit else tokDie],
      maybe [] (\ (ws, x) -> ws ++ [tokLParen] ++ toToks x ++ [tokRParen]) a]
  toToks (ExprExtract ws1 ws2 expr ws3) = concat [[tokExtract], ws1,
    [tokLParen], ws2, toToks expr, ws3, [tokRParen]]
  toToks (ExprInclude incOrReq onceOrNot ws expr) =
    concat [[mainTok], ws, toToks expr]
    where
    mainTok = tokSelf $ mainTokBase ++ onceEnd
    mainTokBase = case incOrReq of
      Inc -> "include"
      Req -> "require"
    onceEnd = case onceOrNot of
      Once -> "_once"
      NotOnce -> ""
  toToks (ExprList a) = toToks a
  toToks (ExprMinusBy var ws1 ws2 expr) =
    exprToToks var ws1 ws2 expr tokMinusBy
  toToks (ExprModBy var ws1 ws2 expr) =
    exprToToks var ws1 ws2 expr tokModBy
  toToks (ExprMulBy var ws1 ws2 expr) =
    exprToToks var ws1 ws2 expr tokMulBy
  toToks (ExprPlusBy var ws1 ws2 expr) =
    exprToToks var ws1 ws2 expr tokPlusBy
  toToks (ExprPrint ws expr) = concat [[tokPrint], ws, toToks expr]
  toToks (ExprShiftLBy var ws1 ws2 expr) =
    exprToToks var ws1 ws2 expr tokShiftLBy
  toToks (ExprShiftRBy var ws1 ws2 expr) =
    exprToToks var ws1 ws2 expr tokShiftRBy
  toToks (ExprTernaryIf a) = toToks a

  toToks (ExprAnd expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokAnd
  toToks (ExprAndWd expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokAndWd
  toToks (ExprArray ws1 content) =
    [tokArray] ++ ws1 ++ [tokLParen] ++ contentToks ++ [tokRParen]
    where
    contentToks = case content of
      Left ws -> ws
      Right (rows, finalComma) -> intercalate [tokComma] (map toToks rows) ++
        maybe [] (tokComma:) finalComma
  toToks (ExprAt ws expr) = concat [[tokAt], ws, toToks expr]
  toToks (ExprBackticks a) = [tokBacktick] ++ toToks a ++ [tokBacktick]
  toToks (ExprBitAnd expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokBitAnd
  toToks (ExprBitNot ws expr) = concat [[tokBitNot], ws, toToks expr]
  toToks (ExprBitOr expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokBitOr
  toToks (ExprBitXor expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokBitXor
  toToks (ExprCast tok ws expr) = concat [[tok], ws, toToks expr]
  toToks (ExprClone ws expr) = concat [[tokClone], ws, toToks expr]
  toToks (ExprConcat expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokConcat
  toToks (ExprConst const) = toToks const
  toToks (ExprDiv expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokDiv
  toToks (ExprEmpty ws1 ws2 expr ws3) = concat [[tokEmpty], ws1,
    [tokLParen], ws2, toToks expr, ws3, [tokRParen]]
  toToks (ExprEQ expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokEQ
  toToks (ExprEval ws1 ws2 expr ws3) = concat [[tokEval], ws1,
    [tokLParen], ws2, toToks expr, ws3, [tokRParen]]
  toToks (ExprFuncCall expr ws args) = concat [toToks expr, ws, [tokLParen],
    argsToks args, [tokRParen]]
  toToks (ExprGE expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokGE
  toToks (ExprGT expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokGT
  toToks (ExprID expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokID
  toToks (ExprIndex expr1 ws1 ws2 expr2 ws3) =
    exprToToks expr1 ws1 ws2 expr2 tokLBracket ++ ws3 ++ [tokRBracket]
  toToks (ExprInstOf expr ws1 ws2 svd) =
    exprToToks expr ws1 ws2 svd tokInstOf
  toToks (ExprIsset ws1 vars) = concat [[tokIsset], ws1,
    [tokLParen], intercalate [tokComma] $ map toToks vars, [tokRParen]]
  toToks (ExprLE expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokLE
  toToks (ExprLT expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokLT
  toToks (ExprMember expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokMember
  toToks (ExprMinus expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokMinus
  toToks (ExprMod expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokMod
  toToks (ExprMul expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokMul
  toToks (ExprNegate ws expr) =
    concat [[tokMinus], ws, toToks expr]
  toToks (ExprNew ws svd args) =
    concat [[tokNew], ws, toToks svd,
    maybe []
      (\ (ws, args) -> ws ++ [tokLParen] ++ argsToks args ++ [tokRParen]) args]
  toToks (ExprNE expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokNE
  toToks (ExprNEOld expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokNEOld
  toToks (ExprNI expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokNI
  toToks (ExprNot ws expr) =
    concat [[tokBang], ws, toToks expr]
  -- FIXME: deal with large-int-becomes-float?  and probably depends on the
  -- machine being 32bit/64bit?
  toToks (ExprNum n) = if any (== '.') n
    then [Tok "LNUMBER" n] else [Tok "DNUMBER" n]
  toToks (ExprOr expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokOr
  toToks (ExprOrWd expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokOrWd
  toToks (ExprParen ws1 expr ws2) = concat [[tokLParen], ws1, toToks expr,
    ws2, [tokRParen]]
  toToks (ExprPlus expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokPlus
  toToks (ExprPos ws expr) = concat [[tokPlus], ws, toToks expr]
  toToks (ExprPostDecr expr ws) = concat [toToks expr, ws, [tokDecr]]
  toToks (ExprPostIncr expr ws) = concat [toToks expr, ws, [tokIncr]]
  toToks (ExprPreDecr ws expr) = concat [[tokDecr], ws, toToks expr]
  toToks (ExprPreIncr ws expr) = concat [[tokIncr], ws, toToks expr]
  toToks (ExprRef ws expr) = concat [[tokRef], ws, toToks expr]
  toToks (ExprStatMemb expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokStatMemb
  toToks (ExprStrLit str) = toToks str
  toToks (ExprShiftL expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokShiftL
  toToks (ExprShiftR expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokShiftR
  toToks (ExprVar var) = toToks var
  toToks (ExprXor expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokXor
  toToks (ExprXorWd expr1 ws1 ws2 expr2) =
    exprToToks expr1 ws1 ws2 expr2 tokXorWd
  toToks (ExprSuppress ws expr) = [tokAt] ++ ws ++ toToks expr

instance ToToks Var where
  toToks (Var a) = [Tok "VARIABLE" ('$':a)]
  toToks (DynVar ws var) = [tokDollar] ++ ws ++ toToks var
  toToks (DynVarExpr ws1 ws2 expr ws3) = [tokDollar] ++ ws1 ++ [tokLBrace] ++
    ws2 ++ toToks expr ++ ws3 ++ [tokRBrace]
  toToks (VarIndex var ws1 expr ws2) = toToks var ++ ws1 ++ [tokLBracket] ++
    toToks expr ++ ws2 ++ [tokRBracket]
  toToks (VarMember var ws1 ws2 memb) = toToks var ++ ws1 ++ [tokMember] ++
    ws2 ++ either (\ x -> [tokLBrace] ++ toToks x ++ [tokRBrace]) toToks memb
  toToks (VarStaticMember c ws1 ws2 var) = toToks c ++ ws1 ++
    [tokStatMemb] ++ ws2 ++ toToks var
  toToks (VarFuncCall sv ws args) = concat [toToks sv, ws, [tokLParen],
    either id (intercalate [tokComma] . map toToks) args, [tokRParen]]
  toToks (VarRef ws var) = concat [[tokRef], ws, toToks var]

instance ToToks Interface where
  toToks (Interface ws1 name ws2 extends block) = concat [
    [tokInterface], ws1, toToks name, ws2,
    if null extends then []
      else tokExtends : intercalate [tokComma] (map toToks extends),
    toToks block]

instance ToToks Class where
  toToks (Class pre ws1 name extends ws2 impls block) = concat [
    toToks pre, [tokClass], ws1, [Tok "STRING" name],
    maybe [] (\ (ws1, ws2, name) -> ws1 ++ [tokExtends] ++ ws2 ++ toToks name)
      extends,
    ws2,
    if null impls then []
      else [tokImplements] ++ intercalate [tokComma] (map toToks impls),
    toToks block]

instance ToToks StmtEnd where
  toToks StmtEndSemi = [tokSemi]
  toToks StmtEndClose = [tokCloseTag]
  toToks StmtEndCloseNL = [tokCloseTagNL]

instance (ToToks a) => ToToks (Block a) where
  toToks (Block a) = [tokLBrace] ++ toToks a ++ [tokRBrace]

instance ToToks DubArrowMb where
  toToks (DubArrowMb Nothing val) = toToks val
  toToks (DubArrowMb (Just (key, ws1, ws2)) val) =
    toToks key ++ ws1 ++ [tokDubArrow] ++ ws2 ++ toToks val

instance ToToks VarMbVal where
  toToks (VarMbVal var exprMb) = toToks var ++ maybe []
    (\ (ws1, ws2, expr) -> ws1 ++ [tokEquals] ++ ws2 ++ toToks expr) exprMb

instance (ToToks a) => ToToks (VarVal a) where
  toToks (VarVal var ws1 ws2 expr) = toToks var ++ ws1 ++ [tokEquals] ++
    ws2 ++ toToks expr

instance (ToToks a) => ToToks (WSCap a) where
  toToks (WSCap a b c) = toToks (a, b, c)

instance ToToks Func where
  toToks (Func ws1 ref name ws2 args ws3 block) = concat [[tokFunction], ws1,
    maybe [] (tokRef:) ref, [Tok "STRING" name], ws2, [tokLParen],
    argsToks args, [tokRParen], ws3, toToks block]

instance ToToks FuncArg where
  toToks (FuncArg ws1 const refWs var defVal ws2) = concat [ws1,
    maybe [] (\ (c, w) -> maybe [tokArray] toToks c ++ w) const,
    maybe [] ((++ [tokRef]) . toToks) refWs, toToks var,
    maybe [] (\ (ws1, ws2, expr) -> ws1 ++ [tokEquals] ++ ws2 ++ toToks expr)
      defVal,
    ws2]

instance ToToks Const where
  toToks (Const s) = [Tok t s] where
    t = case s of
      "__FILE__" -> "FILE"
      "__METHOD__" -> "METHOD_C"
      "__CLASS__" -> "CLASS_C"
      "__FUNCTION__" -> "FUNC_C"
      "__LINE__" -> "LINE"
      -- todo: __NAMESPACE__ and __DIR__ in php 5.3
      _ -> "STRING"
  toToks (ClassConst s ws1 ws2 c) = [Tok "STRING" s] ++ ws1 ++ [tokStatMemb] ++
    ws2 ++ toToks c

instance ToToks StrLit where
  toToks (StrLit str) = [Tok "CONSTANT_ENCAPSED_STRING" str]
  toToks (StrExpr toks ws) = [tokQuote] ++ toToks toks ++ ws ++ [tokQuote]
  toToks (StrHereDoc name toks ws) =
    [Tok "START_HEREDOC" $ "<<<" ++ name ++ "\n"] ++ toToks toks ++ ws ++
    [Tok "END_HEREDOC" name]

instance ParsePos TokWS where
  trackTok (ws, Tok _tokType tokVal) pos =
    updatePosString pos $ concatMap tokGetVal ws ++ tokVal

instance Toklike TokWS where
  selfToTok = snd

wsTokTypes :: [String]
wsTokTypes =
  ["WHITESPACE", "COMMENT", "DOC_COMMENT", "OPEN_TAG", "INLINE_HTML"]

cStmtConstToToks vars = concat [[tokConst], intercalate [tokComma] $
  map toToks vars, [tokSemi]]

cStmtAbsFuncToToks :: [(Tok, WS)] -> WS -> Const -> WS ->
  Either WS [FuncArg] -> WS -> StmtEnd -> WS
cStmtAbsFuncToToks pre ws1 name ws2 args ws3 stmtEnd =
  toToks pre ++ [tokFunction] ++ ws1 ++ toToks name ++ ws2 ++ [tokLParen] ++
  either id (intercalate [tokComma] . map toToks) args ++ [tokRParen] ++
  ws3 ++ toToks stmtEnd

instance WParsable IfaceStmt where
  wParser =
    classConstParser IfaceConst <|>
    classAbsFuncParser IfaceFunc =<< many (tokTypes funcOrVarTypeTokTypes)

exprToToks x1 ws1 ws2 x2 tok =
  concat [toToks x1, ws1, [tok], ws2, toToks x2]

instance WParsable Var where
  wParser = buildExpressionParser varParserTable simpleVarParser

varParserTable :: [[Operator [TokWS] () Identity (WS, Var)]]
varParserTable = [
  -- try seems to be needed, restructure?
  [Prefix . preRep $ try vptStaticMember],
  [Prefix vptRef],
  [Postfix . postRep $ vptMember <|> vptIndex <|> vptFuncCall]]

vptRef :: Parsec [TokWS] () ((WS, Var) -> (WS, Var))
vptRef = do
  (wsPre, _) <- tokEq tokRef
  return $ \ (ws1, var) -> (wsPre, VarRef ws1 var)

vptIndex :: Parsec [TokWS] () ((WS, Var) -> (WS, Var))
vptIndex = do
    a <- fst <$> tokEq tokLBracket
    b <- optionMaybe wParser
    c <- fst <$> tokEq tokRBracket
    return . second $ \ v -> VarIndex (Left v) a b c
  <|> do
    a <- fst <$> tokEq tokLBrace
    b <- optionMaybe wParser
    c <- fst <$> tokEq tokRBrace
    return . second $ \ v -> VarIndex (Left v) a b c

vptMember :: Parsec [TokWS] () ((WS, Var) -> (WS, Var))
vptMember = do
  ws1 <- fst <$> tokEq tokMember
  (ws2, memb) <- second Right <$> wParser <|> second Left <$> braceExprParser
  return . second $ \ v -> VarMember v ws1 ws2 memb

braceExprParser :: Parsec [TokWS] () (WS, WSCap Expr)
braceExprParser = liftM2 (,) (fst <$> tokEq tokLBrace) $ do
  (ws1, expr) <- wParser
  (ws2, _) <- tokEq tokRBrace
  return $ WSCap ws1 expr ws2

vptStaticMember :: Parsec [TokWS] () ((WS, Var) -> (WS, Var))
vptStaticMember = do
  (wsPre, c) <- wParser
  (ws1, _) <- tokEq tokStatMemb
  return $ \ (ws2, v) -> (wsPre, VarStaticMember c ws1 ws2 v)

vptFuncCall :: Parsec [TokWS] () ((WS, Var) -> (WS, Var))
vptFuncCall = do
  (ws1, args) <- argListParser wParser
  return . second $ \ v -> VarFuncCall (Left v) ws1 args

simpleVarParser :: Parsec [TokWS] () (WS, Var)
simpleVarParser =
  second (Var . tail . tokGetVal) <$> tokType "VARIABLE" <|>
  try varFuncCallParser <|>
  try varStaticParser <|>
  liftM2 (,) (fst <$> tokEq tokDollar) (
    uncurry DynVar <$> wParser <|>
    do
      (ws1, _) <- tokEq tokLBrace
      (ws2, expr) <- wParser
      (ws3, _) <- tokEq tokRBrace
      return $ DynVarExpr ws1 ws2 expr ws3
    )

varFuncCallParser :: Parsec [TokWS] () (WS, Var)
varFuncCallParser = do
  (wsPre, funcName) <- wParser
  (ws1, args) <- argListParser wParser
  return (wsPre, VarFuncCall (Right funcName) ws1 args)

varStaticParser :: Parsec [TokWS] () (WS, Var)
varStaticParser = do
  (wsPre, const) <- wParser
  (ws1, _) <- tokEq tokStatMemb
  (ws2, var) <- wParser
  return (wsPre, VarStaticMember const ws1 ws2 var)

argsToks :: (ToToks t, ToToks s) => Either t [s] -> [Tok]
argsToks = either toToks (intercalate [tokComma] . map toToks)

fileParser :: WS -> Parsec [TokWS] () StmtList
fileParser ws = stmtListParser ws <* eof

stmtListParser :: WS -> Parsec [TokWS] () StmtList
stmtListParser wsEnd = do
  stmts <- many wParser
  return $ IC.unbreakEnd stmts wsEnd

instance WParsable SV where
  wParser = second Left <$> wParser <|> second Right <$> wParser

funcLike1Parser :: (WS -> WS -> Expr -> WS -> Expr) ->
  Parsec [TokWS] () (WS, a) -> Parsec [TokWS] () (WS, Expr)
funcLike1Parser constr p = do
  (wsPre, _) <- p
  (ws1, _) <- tokEq tokLParen
  (ws2, expr) <- wParser
  (ws3, _) <- tokEq tokRParen
  return (wsPre, constr ws1 ws2 expr ws3)

funcLikeNParser :: (WS -> WS -> Expr -> WS -> Expr) ->
  Parsec [TokWS] () (WS, a) -> Parsec [TokWS] () (WS, Expr)
funcLikeNParser constr p = do
  (wsPre, _) <- p
  (ws1, _) <- tokEq tokLParen
  (ws2, expr) <- wParser
  (ws3, _) <- tokEq tokRParen
  return (wsPre, constr ws1 ws2 expr ws3)

exitParser :: Parsec [TokWS] () (WS, Expr)
exitParser = do
  (wsPre, isExit) <- (flip (,) True . fst) <$> tokEqNoCase tokExit <|>
    (flip (,) False . fst) <$> tokEqNoCase tokDie
  res <- optionMaybe $ do
    (ws1, _) <- tokEq tokLParen
    mb <- optionMaybe wParser
    (ws3, _) <- tokEq tokRParen
    return $ case mb of
      Nothing -> (ws1, Left ws3)
      Just (ws2, expr) -> (ws1, Right $ WSCap ws2 expr ws3)
  return (wsPre, ExprExit isExit res)

blockOrStmtParser :: Parsec [TokWS] () (WS, Either Stmt (Block Stmt))
blockOrStmtParser = second Right <$> wParser <|>
  second Left <$> wParser

instance Parsable IfBlock where
  parser = do
    (ws1, _) <- tokEq tokLParen
    (ws2, expr) <- wParser
    (ws3, _) <- tokEq tokRParen
    (ws4, block) <- blockOrStmtParser
    return $ IfBlock ws1 ws2 expr ws3 ws4 block

instance WParsable If where
  wParser = liftM2 (,) (fst <$> tokEqNoCase tokIf) (liftM2 If
    (IC.intercalParser parser $
      -- try is needed since we can't distinguish "else if" and "else" hm
      -- todo: can probably reorg to not need it
      second (const Nothing) <$> tokEqNoCase tokElseif <|> try (
        liftM2 (,) (fst <$> tokEqNoCase tokElse)
          ((Just . fst) <$> tokEqNoCase tokIf))) .
    optionMaybe $ do
      (ws1, _) <- tokEqNoCase tokElse
      (ws2, block) <- blockOrStmtParser
      return (ws1, ws2, block))

instance WParsable Foreach where
  wParser = liftM2 (,) (fst <$> tokEqNoCase tokForeach) $ do
    (ws1, _) <- tokEq tokLParen
    (ws2, expr) <- wParser
    (ws3, _) <- tokEqNoCase tokAs
    (ws4, dubArrowMb) <- wParser
    (ws5, _) <- tokEq tokRParen
    (ws6, block) <- blockOrStmtParser
    return $ Foreach ws1 ws2 expr ws3 ws4 dubArrowMb ws5 ws6 block

instance WParsable For where
  wParser = liftM2 (,) (fst <$> tokEqNoCase tokFor) $ do
    -- note: we don't support blockless-if nor colon-syntax
    (ws1, _) <- tokEq tokLParen
    inits <- mbCsvParser wParser $ tokEq tokSemi
    conds <- mbCsvParser wParser $ tokEq tokSemi
    incrs <- mbCsvParser wParser $ tokEq tokRParen
    (ws2, block) <- blockOrStmtParser
    return $ For ws1 inits conds incrs ws2 block

adjustCasesWs :: [(WS, Maybe (WS, Expr), WS, [(WS, Stmt)])] -> WS ->
  (WS, [Case])
adjustCasesWs [] wsEnd = (wsEnd, [])
adjustCasesWs ((wsCPre, header, wsC, wsStmts) : cases) wsEnd =
  (wsCPre, Case header wsC (IC.unbreakEnd wsStmts ws) : cases')
  where
  (ws, cases') = adjustCasesWs cases wsEnd

instance WParsable Switch where
  wParser = liftM2 (,) (fst <$> tokEqNoCase tokSwitch) $ do
    (ws1, _) <- tokEq tokLParen
    (ws2, expr) <- wParser
    (ws3, _) <- tokEq tokRParen
    (ws4, _) <- tokEq tokLBrace
    cases <- many $ do
      (wsCPre, header) <-
        liftM2 (,) (fst <$> tokEqNoCase tokCase) (Just <$> wParser) <|>
        second (const Nothing) <$> tokEqNoCase tokDefault
      (wsC, _) <- tokEq tokColon <|> tokEq tokSemi
      wsStmts <- many wParser
      return (wsCPre, header, wsC, wsStmts)
    (wsEnd, _) <- tokEq tokRBrace
    let (ws5, cases') = adjustCasesWs cases wsEnd
    return $ Switch ws1 ws2 expr ws3 ws4 ws5 cases'

instance WParsable While where
  wParser = liftM2 (,) (fst <$> tokEqNoCase tokWhile) $ do
    (ws1, _) <- tokEq tokLParen
    (ws2, expr) <- wParser
    (ws3, _) <- tokEq tokRParen
    (ws4, block) <- blockOrStmtParser
    return $ While ws1 ws2 expr ws3 ws4 block

instance WParsable DoWhile where
  wParser = liftM2 (,) (fst <$> tokEqNoCase tokDo) $ do
    (ws1, block) <- blockOrStmtParser
    (ws2, _) <- tokEq tokWhile
    (ws3, _) <- tokEq tokLParen
    (ws4, expr) <- wParser
    (ws5, _) <- tokEq tokRParen
    (ws6, stmtEnd) <- wParser
    return $ DoWhile ws1 block ws2 ws3 ws4 expr ws5 ws6 stmtEnd

instance WParsable Declare where
  wParser = liftM2 (,) (fst <$> tokEqNoCase tokDeclare) $ do
    (ws1, _) <- tokEq tokLParen
    (ws2, name) <- wParser
    (ws3, _) <- tokEq tokEquals
    (ws4, expr) <- wParser
    (ws5, _) <- tokEq tokRParen
    (ws6, stmtEnd) <- wParser
    return $ Declare ws1 ws2 name ws3 ws4 expr ws5 ws6 stmtEnd

instance WParsable Stmt where
  wParser = second StmtBlock <$> wParser
    <|> breaklikeParser StmtBreak tokBreak
    <|> breaklikeParser StmtContinue tokContinue
    <|> second StmtClass <$> wParser
    <|> echoParser
    <|> globallikeParser StmtGlobal tokGlobal wParser
    <|> globallikeParser StmtStatic tokStatic wParser
    <|> second StmtIf <$> wParser
    <|> second StmtNothing <$> wParser
    <|> do
      (wsPre, _) <- tokEqNoCase tokUnset
      (ws1, _) <- tokEq tokLParen
      vds <- fst <$> csvParser wParser (tokEq tokRParen)
      (ws2, stmtEnd) <- wParser
      return (wsPre, StmtUnset ws1 vds ws2 stmtEnd)
    <|> breaklikeParser StmtReturn tokReturn
    <|> second StmtForeach <$> wParser
    <|> second StmtFor <$> wParser
    <|> second StmtSwitch <$> wParser
    <|> second StmtFuncDef <$> wParser
    <|> do
      (wsPre, expr) <- wParser
      (wsEnd, stmtEnd) <- wParser
      return (wsPre, StmtExpr expr wsEnd stmtEnd)
    <|> second StmtWhile <$> wParser
    <|> liftM2 (,) (fst <$> tokEqNoCase tokThrow) (do
      (ws1, expr) <- wParser
      (ws2, stmtEnd) <- wParser
      return $ StmtThrow ws1 expr ws2 stmtEnd)
    <|> second StmtDoWhile <$> wParser
    <|> liftM2 (,) (fst <$> tokEqNoCase tokTry) (do
      (ws, block) <- wParser
      catches <- many $ do
        (ws1, _) <- tokEqNoCase tokCatch
        (ws2, _) <- tokEq tokLParen
        (ws3, const) <- wParser
        (ws4, expr) <- wParser
        (ws5, _) <- tokEq tokRParen
        (ws6, block) <- wParser
        return $ Catch ws1 ws2 ws3 const ws4 expr ws5 ws6 block
      return $ StmtTry ws block catches)
    <|> second StmtInterface <$> wParser
    <|> second StmtDeclare <$> wParser

instance WParsable Interface where
  wParser = liftM2 (,) (fst <$> tokEqNoCase tokInterface) $ do
  (ws1, name) <- wParser
  let
    extEnd = do
      (ws2, _) <- tokEqNoCase tokExtends
      (extends, block) <- csvParser wParser $ wParser
      return $ Interface ws1 name ws2 extends block
    extlessEnd = do
      (ws2, block) <- wParser
      return $ Interface ws1 name ws2 [] block
  extEnd <|> extlessEnd

instance WParsable StmtEnd where
  wParser =
    second (const StmtEndSemi) <$> tokEq tokSemi <|>
    second (const StmtEndClose) <$> tokEq tokCloseTag <|>
    second (const StmtEndCloseNL) <$> tokEq tokCloseTagNL

breaklikeParser :: (Maybe (WS, Expr) -> WS -> StmtEnd -> t) -> Tok ->
  Parsec [(WS, Tok)] () (WS, t)
breaklikeParser constr tok = liftM2 (,) (fst <$> tokEqNoCase tok) $ do
  mb <- optionMaybe wParser
  (ws1, stmtEnd) <- wParser
  return $ constr mb ws1 stmtEnd

globallikeParser :: ([WSCap a] -> StmtEnd -> a2) -> Tok ->
  Parsec [TokWS] () (WS, a) -> Parsec [(WS, Tok)] () (WS, a2)
globallikeParser constr tok p = liftM2 (,) (fst <$> tokEqNoCase tok) $
  uncurry constr <$> csvParser p wParser

echoParser :: Parsec [TokWS] () (WS, Stmt)
echoParser = do
  (wsPre, isEcho) <-
    second (const True) <$> tokEqNoCase tokEcho <|>
    second (const False) <$> tokEq tokOpenTagWithEcho
  (exprs, stmtEnd) <- csvParser wParser wParser
  return (wsPre, StmtEcho isEcho exprs stmtEnd)

includeParser :: Tok -> IncOrReq -> OnceOrNot ->
  Parsec [(WS, Tok)] () (WS, Expr)
includeParser tok i o = do
  (wsPre, _) <- tokEqNoCase tok
  (ws1, expr) <- wParser
  return (wsPre, ExprInclude i o ws1 expr)

varValParser :: Parsec [TokWS] () (WS, a) -> Parsec [TokWS] () (WS, VarVal a)
varValParser p = do
  (wsPre, var) <- p
  (ws1, _) <- tokEq tokEquals
  (ws2, val) <- wParser
  return (wsPre, VarVal var ws1 ws2 val)

instance WParsable VarMbVal where
  wParser = do
    (wsPre, var) <- wParser
    val <- optionMaybe $ do
      (ws1, _) <- tokEq tokEquals
      (ws2, expr) <- wParser
      return (ws1, ws2, expr)
    return (wsPre, VarMbVal var val)

instance WParsable Class where
  wParser = do
    pre <- many $ tokTypes ["ABSTRACT", "FINAL"]
    (wsPre, _) <- tokEqNoCase tokClass
    (ws1, Const name) <- wParser
    extends <- optionMaybe $ do
      (wsX1, _) <- tokEqNoCase tokExtends
      (wsX2, nameX) <- wParser
      return (wsX1, wsX2, nameX)
    let
      (wsPre', pre') = reWsPre pre wsPre
      implEnd = do
        (wsY1, _) <- tokEqNoCase tokImplements
        impls <- fst <$> csvParser wParser (tokEq tokLBrace)
        block <- blockEndParser
        return (wsPre', Class pre' ws1 name extends wsY1 impls block)
      implessEnd = do
        (ws2, block) <- wParser
        return (wsPre', Class pre' ws1 name extends ws2 [] block)
    implEnd <|> implessEnd

instance WParsable Func where
  wParser = liftM2 (,) (fst <$> tokEqNoCase tokFunction) $ do
    ref <- optionMaybe $ fst <$> tokEq tokRef
    (ws1, Const name) <- wParser
    (ws2, argsWs) <- second (map funcArgIfy <$>) <$>
      argListParser funcArgParser
    let
      (ws1', ref') = case ref of
        Just ws -> (ws, Just ws1)
        _ -> (ws1, Nothing)
    (ws4, block) <- wParser
    return $ Func ws1' ref' name ws2 argsWs ws4 block

instance WParsable ClassStmt where
  wParser = classConstParser CStmtConst <|> do
    pre <- many $ tokTypes funcOrVarTypeTokTypes
    if any ((== "ABSTRACT") . tokGetType . snd) pre
      then classAbsFuncParser CStmtAbsFunc pre
      else
        case pre of
          [] -> classFuncParser []
          _ -> classFuncParser pre <|> classVarsParser pre

classConstParser :: ([WSCap (VarVal Const)] -> c) ->
  Parsec [(WS, Tok)] () (WS, c)
classConstParser constr = liftM2 (,) (fst <$> tokType "CONST") $
  (constr . fst) <$> (csvParser (varValParser wParser) $ tokEq tokSemi)

rePair :: a -> [(b, a)] -> b -> [(a, b)]
rePair x [] y = [(x, y)]
rePair x ((yM, xM):yxs) y = (x, yM) : rePair xM yxs y

reWsPre :: [(t, t1)] -> t -> (t, [(t1, t)])
reWsPre pre wsPre = case pre of
  [] -> (wsPre, [])
  (pre1Ws, pre1):preRest -> (pre1Ws, rePair pre1 preRest wsPre)

classFuncParser :: [TokWS] -> Parsec [TokWS] () (WS, ClassStmt)
classFuncParser pre = do
  (wsPre, func) <- wParser
  let (wsPre', pre') = reWsPre pre wsPre
  return (wsPre', CStmtFuncDef pre' func)

classAbsFuncParser :: ([(t1, WS)] -> WS -> Const -> WS ->
  Either WS [FuncArg] -> WS -> StmtEnd -> t) -> [(WS, t1)] ->
  Parsec [(WS, Tok)] () (WS, t)
classAbsFuncParser constr pre = do
  (wsPre, _) <- tokEqNoCase tokFunction
  (ws1, name) <- wParser
  (ws2, argsWs) <- second (map funcArgIfy <$>) <$>
    argListParser funcArgParser
  (ws3, stmtEnd) <- wParser
  let (wsPre', pre') = reWsPre pre wsPre
  return (wsPre', constr pre' ws1 name ws2 argsWs ws3 stmtEnd)

classVarsParser :: [TokWS] -> Parsec [TokWS] () (WS, ClassStmt)
classVarsParser pre = do
  let (pre1Ws, pre1):preRest = pre
  (varMbVals, stmtEnd) <- csvParser wParser wParser
  return (pre1Ws, CStmtVar (IC.unbreakStart pre1 preRest) varMbVals stmtEnd)

funcOrVarTypeTokTypes :: [String]
funcOrVarTypeTokTypes =
  ["PROTECTED", "PRIVATE", "PUBLIC", "STATIC", "VAR", "FINAL", "ABSTRACT"]

csvParser :: Parsec [TokWS] () (WS, a) -> Parsec [TokWS] () (WS, b) ->
  Parsec [TokWS] () ([WSCap a], b)
csvParser p end = do
  (colsWsInit, colLast) <- IC.breakEnd <$>
    IC.intercalParser p (fst <$> tokEq tokComma)
  (wsEnd, resEnd) <- end
  return (map (uncurry $ uncurry WSCap) $ colsWsInit ++ [(colLast, wsEnd)],
    resEnd)

mbCsvParser :: Parsec [TokWS] () (WS, a) -> Parsec [TokWS] () (WS, b) ->
  Parsec [TokWS] () (Either WS [WSCap a])
mbCsvParser p end =
  (Right . fst) <$> csvParser p end <|>
  (Left . fst) <$> end

argOrArrListParser :: Bool -> Parsec [TokWS] () (WS, a) ->
  Parsec [TokWS] () (WS, Either WS ([WSCap a], Maybe WS))
argOrArrListParser finalComma p = let
    --grabArg :: Parsec [TokWS] () (Bool, (WSCap a, Maybe WS))
    grabArg = do
      (pWs, pRes) <- p
      delimRes <- (Left . fst) <$> tokEq tokComma <|>
        (Right . fst) <$> tokEq tokRParen
      case delimRes of
        Left ws -> do
          if finalComma
            then do
              endNowRes <- optionMaybe $ fst <$> tokEq tokRParen
              return $ case endNowRes of
                Nothing -> (True, (WSCap pWs pRes ws, Nothing))
                Just wsEnd -> (False, (WSCap pWs pRes ws, Just wsEnd))
            else
              return (True, (WSCap pWs pRes ws, Nothing))
        Right ws -> return (False, (WSCap pWs pRes ws, Nothing))
  in do
    (wsPre, _) <- tokEq tokLParen
    argsWsMb <- optionMaybe $ grow grabArg
    (,) wsPre <$> case argsWsMb of
      Nothing -> (Left . fst) <$> tokEq tokRParen
      Just argsWs -> return $ Right (map fst argsWs, snd $ last argsWs)

grow :: (Monad m) => m (Bool, a) -> m [a]
grow f = do
  (more, fRes) <- f
  if more
    then liftM (fRes:) $ grow f
    else return [fRes]

arrListParser :: Parsec [TokWS] () (WS, a) ->
  Parsec [TokWS] () (WS, Either WS ([WSCap a], Maybe WS))
arrListParser = argOrArrListParser True

argListParser :: Parsec [TokWS] () (WS, a) ->
  Parsec [TokWS] () (WS, Either WS [WSCap a])
argListParser p = second killFinalWs <$> argOrArrListParser False p where
  killFinalWs (Left ws) = Left ws
  killFinalWs (Right (args, _)) = Right args

mbArgListParser :: Parsec [TokWS] () (WS, a) ->
  Parsec [TokWS] () (WS, [Either WS (WSCap a)])
mbArgListParser p = liftM2 (,) (fst <$> tokEq tokLParen) $ grow grabArg where
  grabArg = do
    resArg <- optionMaybe p
    case resArg of
      -- todo: can un-duplciate some stuff here
      Nothing -> do
        resComma <- optionMaybe $ tokEq tokComma
        case resComma of
          Nothing -> do
            (wsEnd, _) <- tokEq tokRParen
            return (False, Left wsEnd)
          Just (wsComma, _) -> do
            return (True, Left wsComma)
      Just (wsArg, arg) -> do
        resComma <- optionMaybe $ tokEq tokComma
        case resComma of
          Nothing -> do
            (wsEnd, _) <- tokEq tokRParen
            return (False, Right $ WSCap wsArg arg wsEnd)
          Just (wsComma, _) -> do
            return (True, Right $ WSCap wsArg arg wsComma)

preRep, postRep :: Parsec [TokWS] () ((WS, a) -> (WS, a)) ->
  Parsec [TokWS] () ((WS, a) -> (WS, a))
preRep  fParse = (fParse >>= \ f -> (f .) <$> preRep  fParse) <|> return id
postRep fParse = (fParse >>= \ f -> (. f) <$> postRep fParse) <|> return id

wsAfter :: (WS -> a -> a) -> Tok ->
  Parsec [TokWS] () ((WS, a) -> (WS, a))
wsAfter  constr tok = do
  (wsPre, _) <- tokEqNoCase tok
  return $ \ (ws1, e) -> (wsPre, constr ws1 e)

wsBefore :: (a -> WS -> a) -> Tok ->
  Parsec [TokWS] () ((WS, a) -> (WS, a))
wsBefore constr tok = do
  (ws1, _) <- tokEqNoCase tok
  return . second $ flip constr ws1

wsInfix :: (a -> WS -> WS -> a -> a) -> Tok ->
  Parsec [TokWS] () ((WS, a) -> (WS, a) -> (WS, a))
wsInfix  constr tok = do
  (ws1, _) <- tokEqNoCase tok
  return $ \ (wsPre, e1) (ws2, e2) -> (wsPre, constr e1 ws1 ws2 e2)

-- FIXME: most of these try's are probably extraneous
exprParserTable :: [[Operator [TokWS] () Identity (WS, Expr)]]
exprParserTable = [
  [Postfix eptFuncCall],
  [Prefix eptClone],
  [Prefix eptPreIncr, Prefix eptPreDecr,
   Postfix eptPostIncr, Postfix eptPostDecr],
  fAll Prefix [eptBitNot, eptNegate, eptPos, eptAt],
  [Postfix eptInstOf],
  [Prefix $ preRep eptNot],
  ial [eptMul, eptDiv, eptMod],
  ial [eptPlus, eptMinus, eptConcat],
  ial [eptShiftL, eptShiftR],
  ian [eptLT, eptLE, eptGT, eptGE, eptNEOld],
  ian [eptEQ, eptNE, eptID, eptNI],
  [Prefix eptRef],
  ial [eptBitAnd],
  ial [eptBitXor],
  ial [eptBitOr],
  [Prefix eptPrint],
  ial [eptAnd],
  ial [eptOr],
  [Postfix eptTernaryIf],
  [Infix eptXBy AssocRight],
  ial [eptAndWd],
  ial [eptXorWd],
  ial [eptOrWd]
  ]

fAll f xs = [f x | x <- xs]

ial, ian :: [Parsec [TokWS] u (a -> a -> a)] ->
  [Operator [TokWS] u Identity a]
ial = fAll $ flip Infix AssocLeft
ian = fAll $ flip Infix AssocNone

eptFuncCall = do
  (ws1, args) <- argListParser wParser
  return . second $ \ e -> ExprFuncCall e ws1 args
eptClone    = wsAfter ExprClone    tokClone

eptPreIncr  = wsAfter  ExprPreIncr  tokIncr
eptPreDecr  = wsAfter  ExprPreDecr  tokDecr
eptPostIncr = wsBefore ExprPostIncr tokIncr
eptPostDecr = wsBefore ExprPostDecr tokDecr
eptBitNot   = wsAfter  ExprBitNot   tokBitNot
eptNegate   = wsAfter  ExprNegate   tokMinus
eptPos      = wsAfter  ExprPos      tokPlus

eptAt     = wsAfter ExprAt     tokAt
eptInstOf = do
  (ws1, _) <- tokEqNoCase tokInstanceOf
  (ws2, sv) <- wParser
  return . second $ \ e -> ExprInstOf e ws1 ws2 sv
eptNot    = wsAfter ExprNot    tokBang
eptMul    = wsInfix ExprMul    tokMul
eptDiv    = wsInfix ExprDiv    tokDiv
eptMod    = wsInfix ExprMod    tokMod
eptPlus   = wsInfix ExprPlus   tokPlus
eptMinus  = wsInfix ExprMinus  tokMinus
eptConcat = wsInfix ExprConcat tokConcat
eptShiftL = wsInfix ExprShiftL tokShiftL
eptShiftR = wsInfix ExprShiftR tokShiftR
eptLT     = wsInfix ExprLT     tokLT
eptLE     = wsInfix ExprLE     tokLE
eptGT     = wsInfix ExprGT     tokGT
eptGE     = wsInfix ExprGE     tokGE
eptNEOld  = wsInfix ExprNEOld  tokNEOld
eptEQ     = wsInfix ExprEQ     tokEQ
eptNE     = wsInfix ExprNE     tokNE
eptID     = wsInfix ExprID     tokID
eptNI     = wsInfix ExprNI     tokNI
eptRef    = wsAfter ExprRef    tokRef
eptBitAnd = wsInfix ExprBitAnd tokBitAnd
eptBitXor = wsInfix ExprBitXor tokBitXor
eptBitOr  = wsInfix ExprBitOr  tokBitOr
eptAnd    = wsInfix ExprAnd    tokAnd
eptOr     = wsInfix ExprOr     tokOr
eptTernaryIf = do
  (ws1, _) <- tokEq tokQuestion
  (ws2, expr1) <- wParser
  (ws3, _) <- tokEq tokColon
  (ws4, expr2) <- wParser
  return $ second
    (\ e -> ExprTernaryIf $ TernaryIf e ws1 ws2 expr1 ws3 ws4 expr2)

eptXBy = do
  (ws1, constr) <-
    second (const ExprAssign)   <$> tokEq tokEquals   <|>
    second (const ExprPlusBy)   <$> tokEq tokPlusBy   <|>
    second (const ExprMinusBy)  <$> tokEq tokMinusBy  <|>
    second (const ExprMulBy)    <$> tokEq tokMulBy    <|>
    second (const ExprDivBy)    <$> tokEq tokDivBy    <|>
    second (const ExprConcatBy) <$> tokEq tokConcatBy <|>
    second (const ExprModBy)    <$> tokEq tokModBy    <|>
    second (const ExprBitAndBy) <$> tokEq tokBitAndBy <|>
    second (const ExprBitOrBy)  <$> tokEq tokBitOrBy  <|>
    second (const ExprBitXorBy) <$> tokEq tokBitXorBy <|>
    second (const ExprShiftLBy) <$> tokEq tokShiftLBy <|>
    second (const ExprShiftRBy) <$> tokEq tokShiftRBy
  return $ \ (wsPre, e1) (ws2, e2) -> (wsPre, constr e1 ws1 ws2 e2)

eptPrint = wsAfter ExprPrint tokPrint
eptAndWd = wsInfix ExprAndWd tokAndWd
eptXorWd = wsInfix ExprXorWd tokXorWd
eptOrWd  = wsInfix ExprOrWd  tokOrWd

instance WParsable Expr where
  wParser = buildExpressionParser exprParserTable simpleExprParser

instance WParsable DubArrowMb where
  wParser = do
    (wsPre, expr1) <- wParser
    expr2Mb <- optionMaybe $ do
      (wsPre, _) <- tokEq tokDubArrow
      (ws1, expr) <- wParser
      return (wsPre, ws1, expr)
    return . (,) wsPre $ case expr2Mb of
      Nothing -> DubArrowMb Nothing expr1
      Just (ws1, ws2, expr2) -> DubArrowMb (Just (expr1, ws1, ws2)) expr2

funclike1Parser :: (WS -> WS -> a -> WS -> b) -> Tok ->
  Parsec [TokWS] () (WS, a) -> Parsec [TokWS] () (WS, b)
funclike1Parser constr tok p = liftM2 (,) (fst <$> tokEqNoCase tok) $ do
  (ws1, _) <- tokEq tokLParen
  (ws2, pRes) <- p
  (ws3, _) <- tokEq tokRParen
  return $ constr ws1 ws2 pRes ws3

simpleExprParser :: Parsec [TokWS] () (WS, Expr)
simpleExprParser = second ExprVar <$> wParser
  <|> second ExprStrLit <$> wParser
  <|> second (ExprNum . tokGetVal) <$> tokTypes ["LNUMBER", "DNUMBER"]
  <|> do
    (wsPre, _) <- tokEq tokLParen
    (ws1, expr) <- wParser
    (ws2, _) <- tokEq tokRParen
    return $ (wsPre, ExprParen ws1 expr ws2)
  <|> do
    (wsPre, _) <- tokEqNoCase tokArray
    (ws1, elems) <- arrListParser wParser
    return (wsPre, ExprArray ws1 elems)
  <|> funclike1Parser ExprEmpty tokEmpty wParser
  <|> includeParser tokInclude Inc NotOnce
  <|> includeParser tokIncludeOnce Inc Once
  <|> includeParser tokRequire Req NotOnce
  <|> includeParser tokRequireOnce Req Once
  <|> liftM2 (,) (fst <$> tokEqNoCase tokIsset) (do
    (ws, _) <- tokEq tokLParen
    vars <- fst <$> csvParser wParser (tokEq tokRParen)
    return $ ExprIsset ws vars)
  <|> do
    (wsPre, _) <- tokEqNoCase tokDefine
    (ws1, _) <- tokEq tokLParen
    (ws2, sv) <- wParser
    (ws3, _) <- tokEq tokComma
    (ws4, expr) <- wParser
    (ws5, _) <- tokEq tokRParen
    return (wsPre, ExprDefine $ Define ws1 ws2 sv ws3 ws4 expr ws5)
  <|> do
    (wsPre, _) <- tokEqNoCase tokList
    (ws1, args) <- mbArgListParser wParser
    (ws2, _) <- tokEq tokEquals
    (ws3, expr) <- wParser
    return (wsPre, ExprList $ List ws1 args ws2 ws3 expr)
  <|> do
    (wsPre, _) <- tokEqNoCase tokNew
    (ws1, sv) <- wParser
    mb <- optionMaybe $ argListParser wParser
    return (wsPre, ExprNew ws1 sv mb)
  <|> do
    (wsPre, t) <- tokTypes (map (++ "_CAST")
      ["INT", "DOUBLE", "STRING", "ARRAY", "OBJECT", "BOOL"])
    (ws1, expr) <- wParser
    return (wsPre, ExprCast t ws1 expr)
  <|> second ExprConst <$> wParser
  <|> liftM2 (,) (fst <$> tokEq tokAt) (do
    (ws1, expr) <- wParser
    return $ ExprSuppress ws1 expr)
  <|> exitParser
  <|> funclike1Parser ExprEval tokEval wParser
  <|> liftM2 (,) (fst <$> tokEq tokBacktick) (do
    toks <- many $ tokNEq tokBacktick
    (ws, _) <- tokEq tokBacktick
    return . ExprBackticks $ IC.unbreakEnd toks ws)

instance WParsable StrLit where
  wParser =
    (second (StrLit . tokGetVal) <$> tokType "CONSTANT_ENCAPSED_STRING")
    <|> liftM2 (,) (fst <$> tokEq tokQuote) (liftM2 StrExpr
      (many $ tokNEq tokQuote)
      (fst <$> tokEq tokQuote))
    <|> do
      (wsPre, _) <- tokType "START_HEREDOC"
      toks <- many $ tokNType "END_HEREDOC"
      (ws, Tok _ name) <- tokType "END_HEREDOC"
      return (wsPre, StrHereDoc name toks ws)

funcArgParser :: Parsec [TokWS] ()
  (WS, (Maybe (Maybe Const, WS), Maybe WS, Var, Maybe (WS, WS, Expr)))
funcArgParser = do
  constMb <- optionMaybe $
    second Just <$> wParser <|>
    second (const Nothing) <$> tokEqNoCase tokArray
  refWs <- optionMaybe $ fst <$> tokEq tokRef
  (wsPre, var) <- wParser
  mbWs1Ws2Expr <- optionMaybe $ do
    (ws1, _) <- tokEq tokEquals
    (ws2, expr) <- wParser
    return (ws1, ws2, expr)
  let
    (wsPre', const') = case constMb of
      Nothing -> (wsPre, Nothing)
      Just (ws, const) -> (ws, Just (const, wsPre))
  return (wsPre', (const', refWs, var, mbWs1Ws2Expr))

funcArgIfy ::
  WSCap (Maybe (Maybe Const, WS), Maybe WS, Var, Maybe (WS, WS, Expr)) ->
  FuncArg
funcArgIfy (WSCap wsA1 (const, refWs, var, mb) wsA2) =
  FuncArg wsA1 const refWs var mb wsA2

-- NOTE: this is unused currently; remove or switch to..
instance WParsable String where
  wParser = second tokGetVal <$> tokType "STRING"

instance WParsable Const where
  wParser = do
    -- todo: __NAMESPACE__ and __DIR__ in php 5.3
    (wsPre, tok) <- tokTypes
      ["STRING", "FILE", "METHOD_C", "CLASS_C", "FUNC_C", "LINE"]
    let
      doneNow = return (wsPre, Const $ tokGetVal tok)
    if tokGetType tok == "STRING"
      then
        try (do
          (ws1, _) <- tokEq tokStatMemb
          (ws2, const) <- wParser
          return (wsPre, ClassConst (tokGetVal tok) ws1 ws2 const))
        <|> doneNow
      else doneNow

instance (WParsable a) => WParsable (Block a) where
  wParser = liftM2 (,) (fst <$> tokEq tokLBrace) blockEndParser

blockEndParser :: (WParsable a) => Parsec [TokWS] () (Block a)
blockEndParser = liftM2 ((Block .) . IC.unbreakEnd)
  (many wParser) (fst <$> tokEq tokRBrace)

$(derive makeBinary ''Block)
$(derive makeBinary ''Case)
$(derive makeBinary ''Catch)
$(derive makeBinary ''Class)
$(derive makeBinary ''ClassStmt)
$(derive makeBinary ''Const)
$(derive makeBinary ''Declare)
$(derive makeBinary ''Define)
$(derive makeBinary ''DoWhile)
$(derive makeBinary ''DubArrowMb)
$(derive makeBinary ''Expr)
$(derive makeBinary ''For)
$(derive makeBinary ''Foreach)
$(derive makeBinary ''Func)
$(derive makeBinary ''FuncArg)
$(derive makeBinary ''If)
$(derive makeBinary ''IfaceStmt)
$(derive makeBinary ''IfBlock)
$(derive makeBinary ''IncOrReq)
$(derive makeBinary ''Interface)
$(derive makeBinary ''List)
$(derive makeBinary ''OnceOrNot)
$(derive makeBinary ''Stmt)
$(derive makeBinary ''StmtEnd)
$(derive makeBinary ''StrLit)
$(derive makeBinary ''Switch)
$(derive makeBinary ''TernaryIf)
$(derive makeBinary ''Var)
$(derive makeBinary ''VarMbVal)
$(derive makeBinary ''VarVal)
$(derive makeBinary ''While)

instance StmtLike Stmt where
  parseAst filename toks = runParser (fileParser wsEnd) () filename toksWs
    where (toksWs, wsEnd) = IC.breakEnd $ absorbWs toks

