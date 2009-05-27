{-# LANGUAGE TemplateHaskell, TypeSynonymInstances #-}

module Ast where

import Control.Applicative ((<$>), (*>), (<*))
import Control.Arrow
import Control.Monad
import Control.Monad.Identity
import Data.Binary
import Data.DeriveTH
import Data.List
import Data.Maybe
import Data.Tok
import FUtil
import Text.Parsec hiding (satisfy, oneOf, noneOf, anyToken)
import Text.Parsec.Expr
import Text.Parsec.Pos
import qualified Data.Intercal as IC

-- abstract syntax tree for a php file

-- we cheat in a few ways for simplicity since we don't care and rigidity can
-- always be added later.  e.g. classes can't actually be nested and you can't
-- actually do "protected $x = 4;" outside a class

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

type StmtList = IC.Intercal WS Stmt

type WS = [Tok]

type TokWS = (WS, Tok)

data WSCap a = WSCap {
  wsCapPre :: WS,
  wsCapMain :: a,
  wsCapPost :: WS
  }
  deriving (Eq, Show)

data Func = Func {
  funcWS1   :: WS,
  funcRef   :: Maybe WS,
  funcName  :: String,
  funcWS2   :: WS,
  funcArgs  :: Either WS [FuncArg],
  funcWS3   :: WS,
  funcBlock :: Block Stmt
  }
  deriving (Eq, Show)

data IfaceStmt =
  IfaceConst [WSCap (VarVal Const)] |
  IfaceFunc [(Tok, WS)] WS Const WS (Either WS [FuncArg]) WS StmtEnd
  deriving (Eq, Show)

data ClassStmt =
  CStmtVar (IC.Intercal Tok WS) [WSCap VarMbVal] StmtEnd |
  CStmtConst [WSCap (VarVal Const)] |
  CStmtFuncDef [(Tok, WS)] Func |
  CStmtAbsFunc [(Tok, WS)] WS Const WS (Either WS [FuncArg]) WS StmtEnd
  deriving (Eq, Show)

data VarMbVal = VarMbVal Var (Maybe (WS, WS, Expr))
  deriving (Eq, Show)

data VarVal a = VarVal a WS WS Expr
  deriving (Eq, Show)

class ModExprs a where
  modExprs :: (Expr -> ([String], Maybe Expr)) -> a -> State (Bool, [String]) a

{-
instance ModExprs Stmt where
  modExprs f (StmtBlock) =
  modExprs f x = x
-}

data Stmt =
  StmtBlock     (Block Stmt)                  |
  StmtBreak     (Maybe (WS, Expr)) WS StmtEnd |
  StmtClass     Class                         |
  StmtContinue  (Maybe (WS, Expr)) WS StmtEnd |
  StmtDeclare   WS WS Const WS WS Expr WS WS StmtEnd |
  StmtDoWhile   WS BlockOrStmt WS WS WS Expr WS WS StmtEnd |
  -- this list must have at least one element.. should i make a type for that?
  StmtEcho      Bool [WSCap Expr] StmtEnd     |
  StmtExpr      Expr WS StmtEnd               |
  StmtFor       WS ForPart ForPart ForPart WS BlockOrStmt |
  StmtForeach   WS WS Expr WS WS DubArrowMb WS WS BlockOrStmt |
  StmtFuncDef   Func                          |
  -- this list must have at least one element.. should i make a type for that?
  StmtGlobal    [WSCap Var] StmtEnd           |
  StmtIf
    (IC.Intercal
      (WS, WS, Expr, WS, WS, BlockOrStmt)
      (WS, Maybe WS)
    )
    (Maybe (WS, WS, BlockOrStmt))             |
  StmtInterface Interface                     |
  StmtNothing   StmtEnd                       |
  StmtReturn    (Maybe (WS, Expr)) WS StmtEnd |
  -- this list must have at least one element.. should i make a type for that?
  StmtStatic    [WSCap VarMbVal] StmtEnd      |
  StmtSwitch    WS WS Expr WS WS
    [(WS, Maybe (WS, Expr), WS, [(WS, Stmt)])]
    WS                                        |
  StmtThrow     WS Expr WS StmtEnd            |
  -- this list must have at least one element.. should i make a type for that?
  StmtTry       WS (Block Stmt)
    [(WS, WS, WS, Const, WS, Expr, WS, WS, Block Stmt)] |
  StmtUnset     WS [WSCap Var] WS StmtEnd     |
  StmtWhile     WS WS Expr WS WS BlockOrStmt
  deriving (Eq, Show)

type ForPart = Either WS [WSCap Expr]

-- statements can end in ";" or not (and just be followed by a "?>")
-- php also oddly distinguishes "?>" and "?>\n", with no other possibilities.
-- (note this differs from e.g. "<?=\n" which is treated as "<?=" + "\n".)
data StmtEnd = StmtEndSemi | StmtEndClose | StmtEndCloseNL
  deriving (Eq, Show)

data Expr =
  -- you would have thought these were statements

  ExprAssign    Expr WS WS Expr               |
  ExprBitAndBy  Expr WS WS Expr               |
  ExprBitOrBy   Expr WS WS Expr               |
  ExprBitXorBy  Expr WS WS Expr               |
  ExprConcatBy  Expr WS WS Expr               |
  ExprDefine    WS WS SV WS WS Expr WS        |
  ExprDivBy     Expr WS WS Expr               |
  ExprExit      Bool (Maybe (WS, Either WS (WSCap Expr))) |
  ExprExtract   WS WS Expr WS                 |
  ExprInclude   IncOrReq OnceOrNot WS Expr    |
  -- "list( ) = array();" is legit php
  ExprList      WS
    [Either WS (WSCap Var)]
    WS WS Expr                                |
  ExprMinusBy   Expr WS WS Expr               |
  ExprModBy     Expr WS WS Expr               |
  ExprMulBy     Expr WS WS Expr               |
  ExprPlusBy    Expr WS WS Expr               |
  ExprPrint     WS Expr                       |
  ExprShiftLBy  Expr WS WS Expr               |
  ExprShiftRBy  Expr WS WS Expr               |
  ExprTernaryIf Expr WS WS Expr WS WS Expr    |

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
  deriving (Eq, Show)

data DubArrowMb = DubArrowMb (Maybe (Expr, WS, WS)) Expr
  deriving (Eq, Show)

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
  deriving (Eq, Show)

type SV = Either Var Const

data Interface = Interface {
  ifaceWS1     :: WS,
  ifaceName    :: Const,
  ifaceWS2     :: WS,
  ifaceExtends :: [WSCap Const],
  ifaceBlock   :: Block IfaceStmt
  }
  deriving (Eq, Show)

-- a block has {}'s, so one-liner's are not considered blocks
-- and a (Block Stmt) is not the same as a StmtList tho it has the same ast
data Block a = Block (IC.Intercal WS a)
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data IncOrReq = Inc | Req
  deriving (Eq, Show)
data OnceOrNot = Once | NotOnce
  deriving (Eq, Show)

-- named records here?
data FuncArg =
  FuncArg WS (Maybe (Maybe Const, WS)) (Maybe WS) Var (Maybe (WS, WS, Expr)) WS
  deriving (Eq, Show)

-- hm is Const overused right now?
data Const = Const String |
  ClassConst String WS WS Const
  deriving (Eq, Show)

data StrLit = StrLit String | StrExpr [TokWS] WS | StrHereDoc String [TokWS] WS
  deriving (Eq, Show)

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

instance ToToks Stmt where
  toToks (StmtBlock a) = toToks a
  toToks (StmtBreak a b stmtEnd) =
    concat [[tokBreak], toToks a, toToks b, toToks stmtEnd]
  toToks (StmtClass a) = toToks a
  toToks (StmtContinue a b stmtEnd) =
    concat [[tokContinue], toToks a, toToks b, toToks stmtEnd]
  toToks (StmtDeclare ws1 ws2 name ws3 ws4 expr ws5 ws6 stmtEnd) =
    concat [[tokDeclare], ws1, [tokLParen], ws2, toToks name, ws3, [tokEquals],
    ws4, toToks expr, ws5, [tokRParen], ws6, toToks stmtEnd]
  toToks (StmtDoWhile ws1 block ws2 ws3 ws4 expr ws5 ws6 stmtEnd) =
    concat [[tokDo], ws1, toToks block, ws2, [tokWhile], ws3, [tokLParen],
    ws4, toToks expr, ws5, [tokRParen], ws6, toToks stmtEnd]
  toToks (StmtEcho isEcho a stmtEnd) = concat [
    [if isEcho then tokEcho else tokOpenTagWithEcho],
    intercalate [tokComma] $ map toToks a, toToks stmtEnd]
  toToks (StmtExpr a b stmtEnd) = concat [toToks a, toToks b, toToks stmtEnd]
  toToks (StmtFor ws1 inits conds incrs ws2 block) = concat [
    [tokFor], ws1, [tokLParen],
    intercalate [tokSemi] $ map csvToToks [inits, conds, incrs], [tokRParen],
    ws2, toToks block]
    where
    csvToToks = either id $ intercalate [tokComma] . map toToks
  toToks (StmtForeach ws1 ws2 expr ws3 ws4 dubArrow ws5 ws6 block) = concat [
    [tokForeach], ws1, [tokLParen], ws2, toToks expr, ws3, [tokAs], ws4,
    toToks dubArrow, ws5, [tokRParen], ws6, toToks block]
  toToks (StmtFuncDef a) = toToks a
  toToks (StmtGlobal [] _) =
    error "global list must have at least one element."
  toToks (StmtGlobal a stmtEnd) = concat [[tokGlobal],
    intercalate [tokComma] $ map toToks a, toToks stmtEnd]
  toToks (StmtIf ifAndIfelses theElse) =
    [tokIf] ++ doCondAndBlock theIf ++
    concatMap doIfelse ifelses ++
    maybe [] (\ (ws1, ws2, block) -> ws1 ++ [tokElse] ++ ws2 ++ toToks block)
      theElse
    where
    (theIf, ifelses) = IC.breakStart ifAndIfelses
    doCondAndBlock (ws1, ws2, expr, ws3, ws4, block) =
      concat [ws1, [tokLParen], ws2, toToks expr, ws3,
      [tokRParen], ws4, toToks block]
    doElsery Nothing = [tokElseif]
    doElsery (Just ws) = [tokElse] ++ ws ++ [tokIf]
    doIfelse ((ws, elsery), condAndBlock) =
      ws ++ doElsery elsery ++ doCondAndBlock condAndBlock
  toToks (StmtInterface a) = toToks a
  toToks (StmtNothing stmtEnd) = toToks stmtEnd
  toToks (StmtReturn a b stmtEnd) =
    concat [[tokReturn], toToks a, toToks b, toToks stmtEnd]
  toToks (StmtSwitch ws1 ws2 expr ws3 ws4 cases ws5) = concat [
    [tokSwitch], ws1, [tokLParen], ws2, toToks expr, ws3, [tokRParen], ws4,
    [tokLBrace], concatMap doCase cases, ws5, [tokRBrace]]
    where
    doCase (wsCPre, header, wsC, stmts) =
      wsCPre ++
      maybe [tokDefault] (\ (ws, expr) -> [tokCase] ++ ws ++ toToks expr)
        header ++
      wsC ++ [tokColon] ++ toToks stmts
  toToks (StmtThrow ws1 expr ws2 stmtEnd) =
    concat [[tokThrow], ws1, toToks expr, ws2, toToks stmtEnd]
  toToks (StmtTry ws block catches) =
    concat [[tokTry], ws, toToks block,
      concatMap (\ (ws1, ws2, ws3, const, ws4, expr, ws5, ws6, block) ->
        ws1 ++ [tokCatch] ++ ws2 ++ [tokLParen] ++ ws3 ++ toToks const ++
        ws4 ++ toToks expr ++ ws5 ++ [tokRParen] ++
        ws6 ++ toToks block) catches]
  toToks (StmtUnset ws1 vds ws2 stmtEnd) =
    concat [[tokUnset], ws1, [tokLParen],
    intercalate [tokComma] $ map toToks vds, [tokRParen], ws2, toToks stmtEnd]
  toToks (StmtStatic [] _) =
    error "static list must have at least one element."
  toToks (StmtStatic a stmtEnd) = concat [[tokStatic],
    intercalate [tokComma] $ map toToks a, toToks stmtEnd]
  toToks (StmtWhile ws1 ws2 expr ws3 ws4 block) =
    concat [[tokWhile], ws1, [tokLParen], ws2, toToks expr, ws3,
      [tokRParen], ws4, toToks block]

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
  toToks (ExprDefine ws1 ws2 svd ws3 ws4 expr ws5) = concat [[tokDefine],
    ws1, [tokLParen], ws2, toToks svd, ws3, [tokComma], ws4, toToks expr,
    ws5, [tokRParen]]
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
  toToks (ExprList ws1 vars ws2 ws3 expr) = concat [[tokList], ws1,
    [tokLParen], intercalate [tokComma] $ map toToks vars,
    [tokRParen], ws2, [tokEquals], ws3, toToks expr]
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
  toToks (ExprTernaryIf expr1 ws1 ws2 expr2 ws3 ws4 expr3) =
    concat [toToks expr1, ws1, [tokQuestion], ws2, toToks expr2, ws3,
    [tokColon], ws4, toToks expr3]

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

absorbWs :: [Tok] -> IC.Intercal WS Tok
absorbWs toks = if null rest
  then IC.Interend ws
  else IC.Intercal ws rest1 $ absorbWs restRest
  where
  (ws, rest) = span ((`elem` wsTokTypes) . tokGetType) toks
  rest1:restRest = rest

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

ifaceStmtParser :: Parsec [TokWS] () (WS, IfaceStmt)
ifaceStmtParser = classConstParser IfaceConst <|>
  classAbsFuncParser IfaceFunc =<< many (tokTypes funcOrVarTypeTokTypes)

exprToToks x1 ws1 ws2 x2 tok =
  concat [toToks x1, ws1, [tok], ws2, toToks x2]

varParser :: Parsec [TokWS] () (WS, Var)
varParser = buildExpressionParser varParserTable simpleVarParser

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
    b <- optionMaybe exprParser
    c <- fst <$> tokEq tokRBracket
    return . second $ \ v -> VarIndex (Left v) a b c
  <|> do
    a <- fst <$> tokEq tokLBrace
    b <- optionMaybe exprParser
    c <- fst <$> tokEq tokRBrace
    return . second $ \ v -> VarIndex (Left v) a b c

vptMember :: Parsec [TokWS] () ((WS, Var) -> (WS, Var))
vptMember = do
  ws1 <- fst <$> tokEq tokMember
  (ws2, memb) <- second Right <$> svParser <|> second Left <$> braceExprParser
  return . second $ \ v -> VarMember v ws1 ws2 memb

braceExprParser :: Parsec [TokWS] () (WS, WSCap Expr)
braceExprParser = liftM2 (,) (fst <$> tokEq tokLBrace) $ do
  (ws1, expr) <- exprParser
  (ws2, _) <- tokEq tokRBrace
  return $ WSCap ws1 expr ws2

vptStaticMember :: Parsec [TokWS] () ((WS, Var) -> (WS, Var))
vptStaticMember = do
  (wsPre, c) <- constParser
  (ws1, _) <- tokEq tokStatMemb
  return $ \ (ws2, v) -> (wsPre, VarStaticMember c ws1 ws2 v)

vptFuncCall :: Parsec [TokWS] () ((WS, Var) -> (WS, Var))
vptFuncCall = do
  (ws1, args) <- argListParser exprParser
  return . second $ \ v -> VarFuncCall (Left v) ws1 args

simpleVarParser :: Parsec [TokWS] () (WS, Var)
simpleVarParser =
  second (Var . tail . tokGetVal) <$> tokType "VARIABLE" <|>
  try varFuncCallParser <|>
  try varStaticParser <|>
  liftM2 (,) (fst <$> tokEq tokDollar) (
    uncurry DynVar <$> varParser <|>
    do
      (ws1, _) <- tokEq tokLBrace
      (ws2, expr) <- exprParser
      (ws3, _) <- tokEq tokRBrace
      return $ DynVarExpr ws1 ws2 expr ws3
    )

varFuncCallParser :: Parsec [TokWS] () (WS, Var)
varFuncCallParser = do
  (wsPre, funcName) <- constParser
  (ws1, args) <- argListParser exprParser
  return (wsPre, VarFuncCall (Right funcName) ws1 args)

varStaticParser :: Parsec [TokWS] () (WS, Var)
varStaticParser = do
  (wsPre, const) <- constParser
  (ws1, _) <- tokEq tokStatMemb
  (ws2, var) <- varParser
  return (wsPre, VarStaticMember const ws1 ws2 var)

argsToks :: (ToToks t, ToToks s) => Either t [s] -> [Tok]
argsToks = either toToks (intercalate [tokComma] . map toToks)

parseAst :: String -> [Tok] -> Either ParseError StmtList
parseAst filename toks = runParser (fileParser wsEnd) () filename toksWs where
  (toksWs, wsEnd) = IC.breakEnd $ absorbWs toks

fileParser :: WS -> Parsec [TokWS] () StmtList
fileParser ws = stmtListParser ws <* eof

stmtListParser :: WS -> Parsec [TokWS] () StmtList
stmtListParser wsEnd = do
  stmts <- many stmtParser
  return $ IC.unbreakEnd stmts wsEnd

svParser :: Parsec [TokWS] () (WS, SV)
svParser = second Left <$> varParser <|> second Right <$> constParser

funcLike1Parser :: (WS -> WS -> Expr -> WS -> Expr) ->
  Parsec [TokWS] () (WS, a) -> Parsec [TokWS] () (WS, Expr)
funcLike1Parser constr p = do
  (wsPre, _) <- p
  (ws1, _) <- tokEq tokLParen
  (ws2, expr) <- exprParser
  (ws3, _) <- tokEq tokRParen
  return (wsPre, constr ws1 ws2 expr ws3)

funcLikeNParser :: (WS -> WS -> Expr -> WS -> Expr) ->
  Parsec [TokWS] () (WS, a) -> Parsec [TokWS] () (WS, Expr)
funcLikeNParser constr p = do
  (wsPre, _) <- p
  (ws1, _) <- tokEq tokLParen
  (ws2, expr) <- exprParser
  (ws3, _) <- tokEq tokRParen
  return (wsPre, constr ws1 ws2 expr ws3)

exitParser :: Parsec [TokWS] () (WS, Expr)
exitParser = do
  (wsPre, isExit) <- (flip (,) True . fst) <$> tokEqNoCase tokExit <|>
    (flip (,) False . fst) <$> tokEqNoCase tokDie
  res <- optionMaybe $ do
    (ws1, _) <- tokEq tokLParen
    mb <- optionMaybe exprParser
    (ws3, _) <- tokEq tokRParen
    return $ case mb of
      Nothing -> (ws1, Left ws3)
      Just (ws2, expr) -> (ws1, Right $ WSCap ws2 expr ws3)
  return (wsPre, ExprExit isExit res)

blockOrStmtParser :: Parsec [TokWS] () (WS, Either Stmt (Block Stmt))
blockOrStmtParser = second Right <$> blockParser stmtParser <|>
  second Left <$> stmtParser

condAndBlockParser ::
  Parsec [TokWS] () (WS, WS, Expr, WS, WS, Either Stmt (Block Stmt))
condAndBlockParser = do
  (ws1, _) <- tokEq tokLParen
  (ws2, expr) <- exprParser
  (ws3, _) <- tokEq tokRParen
  (ws4, block) <- blockOrStmtParser
  return (ws1, ws2, expr, ws3, ws4, block)

stmtParser :: Parsec [TokWS] () (WS, Stmt)
stmtParser = second StmtBlock <$> blockParser stmtParser
  <|> breaklikeParser StmtBreak tokBreak
  <|> breaklikeParser StmtContinue tokContinue
  <|> second StmtClass <$> classParser
  <|> echoParser
  <|> globallikeParser StmtGlobal tokGlobal varParser
  <|> globallikeParser StmtStatic tokStatic varMbValParser
  <|> liftM2 (,) (fst <$> tokEqNoCase tokIf) (liftM2 StmtIf
    (IC.intercalParser condAndBlockParser $
      -- try is needed since we can't distinguish "else if" and "else" hm
      -- todo: can probably reorg to not need it
      second (const Nothing) <$> tokEqNoCase tokElseif <|> try (
        liftM2 (,) (fst <$> tokEqNoCase tokElse)
          ((Just . fst) <$> tokEqNoCase tokIf))) .
    optionMaybe $ do
      (ws1, _) <- tokEqNoCase tokElse
      (ws2, block) <- blockOrStmtParser
      return (ws1, ws2, block))
  <|> second StmtNothing <$> stmtEndParser
  <|> do
    (wsPre, _) <- tokEqNoCase tokUnset
    (ws1, _) <- tokEq tokLParen
    vds <- fst <$> csvParser varParser (tokEq tokRParen)
    (ws2, stmtEnd) <- stmtEndParser
    return (wsPre, StmtUnset ws1 vds ws2 stmtEnd)
  <|> breaklikeParser StmtReturn tokReturn
  <|> do
    (wsPre, _) <- tokEqNoCase tokForeach
    (ws1, _) <- tokEq tokLParen
    (ws2, expr) <- exprParser
    (ws3, _) <- tokEqNoCase tokAs
    (ws4, dubArrowMb) <- dubArrowMbParser
    (ws5, _) <- tokEq tokRParen
    (ws6, block) <- blockOrStmtParser
    return (wsPre, StmtForeach ws1 ws2 expr ws3 ws4 dubArrowMb ws5 ws6 block)
  <|> liftM2 (,) (fst <$> tokEqNoCase tokFor) (do
    -- note: we don't support blockless-if nor colon-syntax
    (ws1, _) <- tokEq tokLParen
    inits <- mbCsvParser exprParser $ tokEq tokSemi
    conds <- mbCsvParser exprParser $ tokEq tokSemi
    incrs <- mbCsvParser exprParser $ tokEq tokRParen
    (ws2, block) <- blockOrStmtParser
    return $ StmtFor ws1 inits conds incrs ws2 block)
  <|> liftM2 (,) (fst <$> tokEqNoCase tokSwitch) (do
    (ws1, _) <- tokEq tokLParen
    (ws2, expr) <- exprParser
    (ws3, _) <- tokEq tokRParen
    (ws4, _) <- tokEq tokLBrace
    cases <- many $ do
      (wsCPre, header) <-
        second Just <$> liftM2 (,) (fst <$> tokEqNoCase tokCase) exprParser <|>
        second (const Nothing) <$> tokEqNoCase tokDefault
      (wsC, _) <- tokEq tokColon
      stmtList <- many stmtParser
      return (wsCPre, header, wsC, stmtList)
    (ws5, _) <- tokEq tokRBrace
    return $ StmtSwitch ws1 ws2 expr ws3 ws4 cases ws5)
  <|> second StmtFuncDef <$> funcParser
  <|> do
    (wsPre, expr) <- exprParser
    (wsEnd, stmtEnd) <- stmtEndParser
    return (wsPre, StmtExpr expr wsEnd stmtEnd)
  <|> liftM2 (,) (fst <$> tokEqNoCase tokWhile) (do
    (ws1, _) <- tokEq tokLParen
    (ws2, expr) <- exprParser
    (ws3, _) <- tokEq tokRParen
    (ws4, block) <- blockOrStmtParser
    return $ StmtWhile ws1 ws2 expr ws3 ws4 block)
  <|> liftM2 (,) (fst <$> tokEqNoCase tokThrow) (do
    (ws1, expr) <- exprParser
    (ws2, stmtEnd) <- stmtEndParser
    return $ StmtThrow ws1 expr ws2 stmtEnd)
  <|> liftM2 (,) (fst <$> tokEqNoCase tokDo) (do
    (ws1, block) <- blockOrStmtParser
    (ws2, _) <- tokEq tokWhile
    (ws3, _) <- tokEq tokLParen
    (ws4, expr) <- exprParser
    (ws5, _) <- tokEq tokRParen
    (ws6, stmtEnd) <- stmtEndParser
    return $ StmtDoWhile ws1 block ws2 ws3 ws4 expr ws5 ws6 stmtEnd)
  <|> liftM2 (,) (fst <$> tokEqNoCase tokTry) (do
    (ws, block) <- blockParser stmtParser
    catches <- many $ do
      (ws1, _) <- tokEqNoCase tokCatch
      (ws2, _) <- tokEq tokLParen
      (ws3, const) <- constParser
      (ws4, expr) <- exprParser
      (ws5, _) <- tokEq tokRParen
      (ws6, block) <- blockParser stmtParser
      return (ws1, ws2, ws3, const, ws4, expr, ws5, ws6, block)
    return $ StmtTry ws block catches)
  <|> second StmtInterface <$> interfaceParser
  <|> liftM2 (,) (fst <$> tokEqNoCase tokDeclare) (do
    (ws1, _) <- tokEq tokLParen
    (ws2, name) <- constParser
    (ws3, _) <- tokEq tokEquals
    (ws4, expr) <- exprParser
    (ws5, _) <- tokEq tokRParen
    (ws6, stmtEnd) <- stmtEndParser
    return $ StmtDeclare ws1 ws2 name ws3 ws4 expr ws5 ws6 stmtEnd)

interfaceParser :: Parsec [TokWS] () (WS, Interface)
interfaceParser = liftM2 (,) (fst <$> tokEqNoCase tokInterface) $ do
  (ws1, name) <- constParser
  let
    extEnd = do
      (ws2, _) <- tokEqNoCase tokExtends
      (extends, block) <- csvParser constParser $ blockParser ifaceStmtParser
      return $ Interface ws1 name ws2 extends block
    extlessEnd = do
      (ws2, block) <- blockParser ifaceStmtParser
      return $ Interface ws1 name ws2 [] block
  extEnd <|> extlessEnd

stmtEndParser :: Parsec [TokWS] () (WS, StmtEnd)
stmtEndParser =
  second (const StmtEndSemi) <$> tokEq tokSemi <|>
  second (const StmtEndClose) <$> tokEq tokCloseTag <|>
  second (const StmtEndCloseNL) <$> tokEq tokCloseTagNL

breaklikeParser :: (Maybe (WS, Expr) -> WS -> StmtEnd -> t) -> Tok ->
  Parsec [(WS, Tok)] () (WS, t)
breaklikeParser constr tok = liftM2 (,) (fst <$> tokEqNoCase tok) $ do
  mb <- optionMaybe exprParser
  (ws1, stmtEnd) <- stmtEndParser
  return $ constr mb ws1 stmtEnd

globallikeParser :: ([WSCap a] -> StmtEnd -> a2) -> Tok ->
  Parsec [TokWS] () (WS, a) -> Parsec [(WS, Tok)] () (WS, a2)
globallikeParser constr tok p = liftM2 (,) (fst <$> tokEqNoCase tok) $
  uncurry constr <$> csvParser p stmtEndParser

echoParser :: Parsec [TokWS] () (WS, Stmt)
echoParser = do
  (wsPre, isEcho) <-
    second (const True) <$> tokEqNoCase tokEcho <|>
    second (const False) <$> tokEq tokOpenTagWithEcho
  (exprs, stmtEnd) <- csvParser exprParser stmtEndParser
  return (wsPre, StmtEcho isEcho exprs stmtEnd)

includeParser :: Tok -> IncOrReq -> OnceOrNot ->
  Parsec [(WS, Tok)] () (WS, Expr)
includeParser tok i o = do
  (wsPre, _) <- tokEqNoCase tok
  (ws1, expr) <- exprParser
  return (wsPre, ExprInclude i o ws1 expr)

varValParser :: Parsec [TokWS] () (WS, a) -> Parsec [TokWS] () (WS, VarVal a)
varValParser p = do
  (wsPre, var) <- p
  (ws1, _) <- tokEq tokEquals
  (ws2, val) <- exprParser
  return (wsPre, VarVal var ws1 ws2 val)

varMbValParser :: Parsec [TokWS] () (WS, VarMbVal)
varMbValParser = do
  (wsPre, var) <- varParser
  val <- optionMaybe $ do
    (ws1, _) <- tokEq tokEquals
    (ws2, expr) <- exprParser
    return (ws1, ws2, expr)
  return (wsPre, VarMbVal var val)

classParser :: Parsec [TokWS] () (WS, Class)
classParser = do
  pre <- many $ tokTypes ["ABSTRACT", "FINAL"]
  (wsPre, _) <- tokEqNoCase tokClass
  (ws1, Const name) <- constParser
  extends <- optionMaybe $ do
    (wsX1, _) <- tokEqNoCase tokExtends
    (wsX2, nameX) <- constParser
    return (wsX1, wsX2, nameX)
  let
    (wsPre', pre') = reWsPre pre wsPre
    implEnd = do
      (wsY1, _) <- tokEqNoCase tokImplements
      impls <- fst <$> csvParser constParser (tokEq tokLBrace)
      block <- blockEndParser classStmtParser
      return (wsPre', Class pre' ws1 name extends wsY1 impls block)
    implessEnd = do
      (ws2, block) <- blockParser classStmtParser
      return (wsPre', Class pre' ws1 name extends ws2 [] block)
  implEnd <|> implessEnd

funcParser :: Parsec [TokWS] () (WS, Func)
funcParser = liftM2 (,) (fst <$> tokEqNoCase tokFunction) $ do
  ref <- optionMaybe $ fst <$> tokEq tokRef
  (ws1, Const name) <- constParser
  (ws2, argsWs) <- second (map funcArgIfy <$>) <$>
    argListParser funcArgParser
  let
    (ws1', ref') = case ref of
      Just ws -> (ws, Just ws1)
      _ -> (ws1, Nothing)
  (ws4, block) <- blockParser stmtParser
  return $ Func ws1' ref' name ws2 argsWs ws4 block

classStmtParser :: Parsec [TokWS] () (WS, ClassStmt)
classStmtParser = classConstParser CStmtConst <|> do
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
  (constr . fst) <$> (csvParser (varValParser constParser) $ tokEq tokSemi)

rePair :: a -> [(b, a)] -> b -> [(a, b)]
rePair x [] y = [(x, y)]
rePair x ((yM, xM):yxs) y = (x, yM) : rePair xM yxs y

reWsPre :: [(t, t1)] -> t -> (t, [(t1, t)])
reWsPre pre wsPre = case pre of
  [] -> (wsPre, [])
  (pre1Ws, pre1):preRest -> (pre1Ws, rePair pre1 preRest wsPre)

classFuncParser :: [TokWS] -> Parsec [TokWS] () (WS, ClassStmt)
classFuncParser pre = do
  (wsPre, func) <- funcParser
  let (wsPre', pre') = reWsPre pre wsPre
  return (wsPre', CStmtFuncDef pre' func)

classAbsFuncParser :: ([(t1, WS)] -> WS -> Const -> WS ->
  Either WS [FuncArg] -> WS -> StmtEnd -> t) -> [(WS, t1)] ->
  Parsec [(WS, Tok)] () (WS, t)
classAbsFuncParser constr pre = do
  (wsPre, _) <- tokEqNoCase tokFunction
  (ws1, name) <- constParser
  (ws2, argsWs) <- second (map funcArgIfy <$>) <$>
    argListParser funcArgParser
  (ws3, stmtEnd) <- stmtEndParser
  let (wsPre', pre') = reWsPre pre wsPre
  return (wsPre', constr pre' ws1 name ws2 argsWs ws3 stmtEnd)

classVarsParser :: [TokWS] -> Parsec [TokWS] () (WS, ClassStmt)
classVarsParser pre = do
  let (pre1Ws, pre1):preRest = pre
  (varMbVals, stmtEnd) <- csvParser varMbValParser $ stmtEndParser
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
  (ws1, args) <- argListParser exprParser
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
  (ws2, sv) <- svParser
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
  (ws2, expr1) <- exprParser
  (ws3, _) <- tokEq tokColon
  (ws4, expr2) <- exprParser
  return . second $ \ e -> ExprTernaryIf e ws1 ws2 expr1 ws3 ws4 expr2

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

exprParser :: Parsec [TokWS] () (WS, Expr)
exprParser = buildExpressionParser exprParserTable simpleExprParser

dubArrowMbParser :: Parsec [TokWS] () (WS, DubArrowMb)
dubArrowMbParser = do
  (wsPre, expr1) <- exprParser
  expr2Mb <- optionMaybe $ do
    (wsPre, _) <- tokEq tokDubArrow
    (ws1, expr) <- exprParser
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
simpleExprParser = second ExprVar <$> varParser
  <|> second ExprStrLit <$> strLitParser
  <|> second (ExprNum . tokGetVal) <$> tokTypes ["LNUMBER", "DNUMBER"]
  <|> do
    (wsPre, _) <- tokEq tokLParen
    (ws1, expr) <- exprParser
    (ws2, _) <- tokEq tokRParen
    return $ (wsPre, ExprParen ws1 expr ws2)
  <|> do
    (wsPre, _) <- tokEqNoCase tokArray
    (ws1, elems) <- arrListParser dubArrowMbParser
    return (wsPre, ExprArray ws1 elems)
  <|> funclike1Parser ExprEmpty tokEmpty varParser
  <|> includeParser tokInclude Inc NotOnce
  <|> includeParser tokIncludeOnce Inc Once
  <|> includeParser tokRequire Req NotOnce
  <|> includeParser tokRequireOnce Req Once
  <|> liftM2 (,) (fst <$> tokEqNoCase tokIsset) (do
    (ws, _) <- tokEq tokLParen
    vars <- fst <$> csvParser varParser (tokEq tokRParen)
    return $ ExprIsset ws vars)
  <|> do
    (wsPre, _) <- tokEqNoCase tokDefine
    (ws1, _) <- tokEq tokLParen
    (ws2, sv) <- svParser
    (ws3, _) <- tokEq tokComma
    (ws4, expr) <- exprParser
    (ws5, _) <- tokEq tokRParen
    return (wsPre, ExprDefine ws1 ws2 sv ws3 ws4 expr ws5)
  <|> do
    (wsPre, _) <- tokEqNoCase tokList
    (ws1, args) <- mbArgListParser varParser
    (ws2, _) <- tokEq tokEquals
    (ws3, expr) <- exprParser
    return (wsPre, ExprList ws1 args ws2 ws3 expr)
  <|> do
    (wsPre, _) <- tokEqNoCase tokNew
    (ws1, sv) <- svParser
    mb <- optionMaybe $ argListParser exprParser
    return (wsPre, ExprNew ws1 sv mb)
  <|> do
    (wsPre, t) <- tokTypes (map (++ "_CAST")
      ["INT", "DOUBLE", "STRING", "ARRAY", "OBJECT", "BOOL"])
    (ws1, expr) <- exprParser
    return (wsPre, ExprCast t ws1 expr)
  <|> second ExprConst <$> constParser
  <|> liftM2 (,) (fst <$> tokEq tokAt) (do
    (ws1, expr) <- exprParser
    return $ ExprSuppress ws1 expr)
  <|> exitParser
  <|> funclike1Parser ExprEval tokEval exprParser
  <|> liftM2 (,) (fst <$> tokEq tokBacktick) (do
    toks <- many $ tokNEq tokBacktick
    (ws, _) <- tokEq tokBacktick
    return . ExprBackticks $ IC.unbreakEnd toks ws)

strLitParser :: Parsec [TokWS] () (WS, StrLit)
strLitParser =
  (second (StrLit . tokGetVal) <$>
    tokType "CONSTANT_ENCAPSED_STRING")
  <|> liftM2 (,) (fst <$> tokEq tokQuote) (liftM2 StrExpr
    (many $ tokNEq tokQuote)
    (fst <$> tokEq tokQuote))
  <|> do
    (wsPre, _) <- tokType "START_HEREDOC"
    toks <- many $ tokNType "END_HEREDOC"
    (ws, Tok _ name) <- tokType "END_HEREDOC"
    return (wsPre, StrHereDoc name toks ws)

funcArgParser :: Parsec [TokWS] () (WS, (Maybe (Maybe Const, WS), Maybe WS, Var,
  Maybe (WS, WS, Expr)))
funcArgParser = do
  constMb <- optionMaybe $
    second Just <$> constParser <|>
    second (const Nothing) <$> tokEqNoCase tokArray
  refWs <- optionMaybe $ fst <$> tokEq tokRef
  (wsPre, var) <- varParser
  mbWs1Ws2Expr <- optionMaybe $ do
    (ws1, _) <- tokEq tokEquals
    (ws2, expr) <- exprParser
    return (ws1, ws2, expr)
  let
    (wsPre', const') = case constMb of
      Nothing -> (wsPre, Nothing)
      Just (ws, const) -> (ws, Just (const, wsPre))
  return (wsPre', (const', refWs, var, mbWs1Ws2Expr))

-- type for funcArgIfy could not be autogenerated
funcArgIfy (WSCap wsA1 (const, refWs, var, mb) wsA2) =
  FuncArg wsA1 const refWs var mb wsA2

strParser :: Parsec [TokWS] () (WS, String)
strParser = second tokGetVal <$> tokType "STRING"

constParser :: Parsec [TokWS] () (WS, Const)
constParser = do
  -- todo: __NAMESPACE__ and __DIR__ in php 5.3
  (wsPre, tok) <- tokTypes
    ["STRING", "FILE", "METHOD_C", "CLASS_C", "FUNC_C", "LINE"]
  let
    doneNow = return (wsPre, Const $ tokGetVal tok)
  if tokGetType tok == "STRING"
    then
      try (do
        (ws1, _) <- tokEq tokStatMemb
        (ws2, const) <- constParser
        return (wsPre, ClassConst (tokGetVal tok) ws1 ws2 const))
      <|> doneNow
    else doneNow

blockParser :: Parsec [TokWS] () (WS, a) -> Parsec [TokWS] () (WS, Block a)
blockParser = liftM2 (,) (fst <$> tokEq tokLBrace) . blockEndParser

blockEndParser :: Parsec [TokWS] () (WS, a) -> Parsec [TokWS] () (Block a)
blockEndParser p = do
  stmtList <- many p
  (wsPost, _) <- tokEq tokRBrace
  return . Block $ IC.unbreakEnd stmtList wsPost

$(derive makeBinary ''Block)
$(derive makeBinary ''Class)
$(derive makeBinary ''ClassStmt)
$(derive makeBinary ''Const)
$(derive makeBinary ''DubArrowMb)
$(derive makeBinary ''Expr)
$(derive makeBinary ''Func)
$(derive makeBinary ''FuncArg)
$(derive makeBinary ''IfaceStmt)
$(derive makeBinary ''IncOrReq)
$(derive makeBinary ''Interface)
$(derive makeBinary ''OnceOrNot)
$(derive makeBinary ''Stmt)
$(derive makeBinary ''StmtEnd)
$(derive makeBinary ''StrLit)
$(derive makeBinary ''Tok)
$(derive makeBinary ''Var)
$(derive makeBinary ''VarMbVal)
$(derive makeBinary ''VarVal)
$(derive makeBinary ''WSCap)
