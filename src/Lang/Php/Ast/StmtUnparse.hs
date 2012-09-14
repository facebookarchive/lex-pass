-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Lang.Php.Ast.StmtUnparse where

import Control.Monad.Identity
import Lang.Php.Ast.ArgList
import Lang.Php.Ast.Common
import Lang.Php.Ast.Lex
import Lang.Php.Ast.StmtTypes
import Text.ParserCombinators.Parsec.Expr
import qualified Data.Intercal as IC

-- Val

instance Unparse Var where
  unparse (Var s indexes) = tokDollar ++ s ++
    concatMap (\ (ws, (isBracket, expr)) -> unparse ws ++
      if isBracket
        then tokLBracket ++ unparse expr ++ tokRBracket
        else tokLBrace ++ unparse expr ++ tokRBrace
      ) indexes
  unparse (VarDyn ws var) = tokDollar ++ unparse ws ++ unparse var
  unparse (VarDynExpr ws expr) = tokDollar ++ unparse ws ++ tokLBrace ++
    unparse expr ++ tokRBrace

instance Unparse Const where
  unparse (Const statics s) = concatMap (\ (s, (ws1, ws2)) -> s ++
    unparse ws1 ++ tokDubColon ++ unparse ws2) statics ++ s

instance Unparse DynConst where
  unparse (DynConst statics var) = concatMap (\ (s, (ws1, ws2)) -> s ++
    unparse ws1 ++ tokDubColon ++ unparse ws2) statics ++ unparse var

instance Unparse LRVal where
  unparse (LRValVar a) = unparse a
  unparse (LRValInd a w e) = unparse a ++ unparse w ++ tokLBracket ++
    unparse e ++ tokRBracket
  unparse (LRValMemb v (ws1, ws2) m) =
    unparse v ++ unparse ws1 ++ tokArrow ++ unparse ws2 ++ unparse m
  unparse (LRValStaMemb v (ws1, ws2) m) =
    unparse v ++ unparse ws1 ++ tokDubColon ++ unparse ws2 ++ unparse m

instance Unparse LOnlyVal where
  unparse (LOnlyValList w args) = tokList ++ unparse w ++ tokLParen ++
    either unparse (intercalate tokComma . map unparse) args ++ tokRParen
  unparse (LOnlyValAppend v (ws1, ws2)) =
    unparse v ++ unparse ws1 ++ tokLBracket ++ unparse ws2 ++ tokRBracket
  unparse (LOnlyValInd v ws expr) =
    unparse v ++ unparse ws ++ tokLBracket ++ unparse expr ++ tokRBracket
  unparse (LOnlyValMemb v (ws1, ws2) m) =
    unparse v ++ unparse ws1 ++ tokArrow ++ unparse ws2 ++ unparse m

instance Unparse ROnlyVal where
  unparse (ROnlyValConst a) = unparse a
  unparse (ROnlyValFunc v ws (Left w)) = unparse v ++ unparse ws ++
    tokLParen ++ unparse w ++ tokRParen
  unparse (ROnlyValFunc v ws (Right args)) = unparse v ++ unparse ws ++
    tokLParen ++ intercalate tokComma (map unparse args) ++ tokRParen

instance Unparse Memb where
  unparse (MembExpr e) = tokLBrace ++ unparse e ++ tokRBrace
  unparse (MembStr s) = s
  unparse (MembVar a) = unparse a

instance Unparse Val where
  unparse (ValLOnlyVal a) = unparse a
  unparse (ValROnlyVal a) = unparse a
  unparse (ValLRVal a) = unparse a

instance Unparse LVal where
  unparse (LValLOnlyVal a) = unparse a
  unparse (LValLRVal a) = unparse a

instance Unparse RVal where
  unparse (RValROnlyVal a) = unparse a
  unparse (RValLRVal a) = unparse a

-- Expr

instance Unparse Expr where
  unparse expr = case expr of
    ExprAnonFunc a -> unparse a
    ExprArray w elemsOrW -> tokArray ++ unparse w ++ tokLParen ++
      either unparse f elemsOrW ++ tokRParen where
      f (elems, wEnd) = intercalate tokComma .
        maybe id (flip (++) . (:[]) . unparse) wEnd $ map unparse elems
    ExprAssign o v w e -> unparse v ++ w2With (unparse o ++ tokEquals) w ++
      unparse e
    ExprBackticks a -> a
    ExprBinOp o e1 (w1, w2) e2 -> unparse e1 ++ unparse w1 ++ unparse o ++
      unparse w2 ++ unparse e2
    ExprCast (WSCap w1 t w2) w e -> tokLParen ++ unparse w1 ++ t ++
      unparse w2 ++ tokRParen ++ unparse w ++ unparse e
    ExprEmpty w e -> tokEmpty ++ unparse w ++ tokLParen ++ unparse e ++
      tokRParen
    ExprEval w e -> tokEval ++ unparse w ++ tokLParen ++ unparse e ++
      tokRParen
    ExprExit isExit a -> (if isExit then tokExit else tokDie) ++
      maybe "" (\ (w, x) -> unparse w ++ tokLParen ++
        either unparse unparse x ++ tokRParen) a
    ExprHereDoc a -> unparse a
    ExprInclude a b w e -> unparse a ++ unparse b ++ unparse w ++ unparse e
    ExprIndex a w b ->
      unparse a ++ unparse w ++ tokLBracket ++ unparse b ++ tokRBracket
    ExprInstOf e w t -> unparse e ++ w2With tokInstanceof w ++ unparse t
    ExprIsset w vs -> tokIsset ++ unparse w ++ tokLParen ++
      intercalate tokComma (map unparse vs) ++ tokRParen
    ExprNew w a argsMb -> tokNew ++ unparse w ++ unparse a ++ maybe ""
      (\ (wPre, args) -> unparse wPre ++ tokLParen ++ either unparse
        (intercalate tokComma . map unparse) args ++ tokRParen) argsMb
    ExprNumLit a -> unparse a
    ExprParen a -> tokLParen ++ unparse a ++ tokRParen
    ExprPostOp o e w -> unparse e ++ unparse w ++ unparse o
    ExprPreOp o w e -> unparse o ++ unparse w ++ unparse e
    ExprRef w v -> tokAmp ++ unparse w ++ unparse v
    ExprRVal a -> unparse a
    ExprStrLit a -> unparse a
    ExprTernaryIf a -> unparse a
    ExprXml a -> unparse a

instance Unparse BinOpBy where
  unparse binOp = case binOp of
    BBitAnd -> tokAmp
    BBitOr -> tokBitOr
    BConcat -> tokConcat
    BDiv -> tokDiv
    BMinus -> tokMinus
    BMod -> tokMod
    BMul -> tokMul
    BPlus -> tokPlus
    BShiftL -> tokShiftL
    BShiftR -> tokShiftR
    BXor -> tokXor

instance Unparse BinOp where
  unparse binOp = case binOp of
    BAnd -> tokAnd
    BAndWd -> tokAndWd
    BEQ -> tokEQ
    BGE -> tokGE
    BGT -> tokGT
    BID -> tokID
    BLE -> tokLE
    BLT -> tokLT
    BNE -> tokNE
    BNEOld -> tokNEOld
    BNI -> tokNI
    BOr -> tokOr
    BOrWd -> tokOrWd
    BXorWd -> tokXorWd
    BByable o -> unparse o

instance Unparse PreOp where
  unparse preOp = case preOp of
    PrPrint -> tokPrint
    PrAt -> tokAt
    PrBitNot -> tokBitNot
    PrClone -> tokClone
    PrNegate -> tokMinus
    PrNot -> tokNot
    PrPos -> tokPlus
    PrSuppress -> tokAt
    PrIncr -> tokIncr
    PrDecr -> tokDecr

instance Unparse PostOp where
  unparse postOp = case postOp of
    PoIncr -> tokIncr
    PoDecr -> tokDecr

instance Unparse IncOrReq where
  unparse Inc = tokInclude
  unparse Req = tokRequire

instance Unparse OnceOrNot where
  unparse Once = "_once"
  unparse NotOnce = ""

instance Unparse DubArrowMb where
  unparse (DubArrowMb k v) = maybe "" (\ (e, (w1, w2)) -> unparse e ++
    unparse w1 ++ tokDubArrow ++ unparse w2) k ++ unparse v

instance Unparse TernaryIf where
  unparse (TernaryIf e1 (w1, w2) e2 (w3, w4) e3) = unparse e1 ++ unparse w1 ++
    tokQMark ++ unparse w2 ++ unparse e2 ++ unparse w3 ++ tokColon ++
    unparse w4 ++ unparse e3

instance Unparse Xml where
  unparse (Xml tag attrs content) = tokLT ++ tag ++
    IC.intercalUnparser unparse
      (\ (k, vMb) -> k ++
        maybe "" (\ (w, v) -> w2With tokEquals w ++
        either unparse ((tokLBrace ++) . (++ tokRBrace) . unparse) v) vMb)
      attrs ++
    maybe tokDiv (\ (c, hasExplicitCloseTag) ->
      tokGT ++ concatMap unparse c ++ tokLT ++ tokDiv ++
      if hasExplicitCloseTag then tag else "") content ++
    tokGT

instance Unparse XmlLitOrExpr where
  unparse (XmlLit a) = a
  unparse (XmlExpr a) = tokLBrace ++ unparse a ++ tokRBrace

instance Unparse VarMbVal where
  unparse (VarMbVal var exprMb) = unparse var ++ maybe []
    (\ (w, expr) -> w2With tokEquals w ++ unparse expr) exprMb

instance Unparse FuncArg where
  unparse (FuncArg const refWs var) = concat [
    maybe [] (\ (c, w) -> maybe tokArray unparse c ++ unparse w) const,
    maybe [] ((tokAmp ++) . unparse) refWs, unparse var]

instance Unparse AnonFuncUse where
  unparse (AnonFuncUse argList) = tokUse ++ unparse argList

instance Unparse AnonFunc where
  unparse (AnonFunc w1 ref (WSCap w2 args w3) use block) = concat [tokFunction,
    unparse w1, maybe [] ((tokAmp ++) . unparse) ref, unparse w2,
    tokLParen, unparse args, tokRParen, unparse w3, unparse use, unparse block]

-- Stmt

instance Unparse Stmt where
  unparse stmt = case stmt of
    StmtBlock a -> unparse a
    StmtBreak iMb w end -> tokBreak ++ unparse iMb ++ unparse w ++ unparse end
    StmtClass a -> unparse a
    StmtContinue iMb w end -> tokContinue ++ unparse iMb ++ unparse w ++
      unparse end
    StmtDeclare a -> unparse a
    StmtDoWhile a -> unparse a
    StmtEcho a end -> tokEcho ++ intercalate tokComma (map unparse a) ++
      unparse end
    StmtExpr a b c -> unparse a ++ unparse b ++ unparse c
    StmtFor a -> unparse a
    StmtForeach a -> unparse a
    StmtFuncDef a -> unparse a
    StmtGlobal a end -> tokGlobal ++
      intercalate tokComma (map unparse a) ++ unparse end
    StmtIf a -> unparse a
    StmtInterface a -> unparse a
    StmtNamespace n end -> tokNamespace ++ unparse n ++ unparse end
    StmtNothing end -> unparse end
    StmtReturn rMb w end -> tokReturn ++ unparse rMb ++ unparse w ++
      unparse end
    StmtStatic a end -> tokStatic ++ intercalate tokComma (map unparse a) ++
      unparse end
    StmtSwitch a -> unparse a
    StmtThrow a end -> tokThrow ++ unparse a ++ unparse end
    StmtTry a cs -> tokTry ++ unparse a ++ unparse cs
    StmtUnset (WSCap w1 a w2) end -> tokUnset ++ unparse w1 ++ tokLParen ++
      intercalate tokComma (map unparse a) ++ tokRParen ++ unparse w2 ++
      unparse end
    StmtUse n end -> tokUse ++ unparse n ++ unparse end
    StmtWhile a -> unparse a

instance Unparse StmtEnd where
  unparse StmtEndSemi = tokSemi
  unparse (StmtEndClose a) = tokClosePhp ++ unparse a

instance Unparse TopLevel where
  unparse (TopLevel s echoOrTok) = s ++
    maybe "" (either ((tokOpenPhpEcho ++) . unparse) ("<?" ++)) echoOrTok

instance (Unparse a) => Unparse (Block a) where
  unparse (Block a) = tokLBrace ++ unparse a ++ tokRBrace

unparsePre :: [(String, WS)] -> String
unparsePre = concatMap (\ (a, b) -> a ++ unparse b)

instance Unparse Class where
  unparse (Class pre (WSCap w1 name w2) extends impls block) = concat [
    unparsePre pre, tokClass, unparse w1, name, unparse w2,
    maybe [] ((tokExtends ++) . unparse) extends,
    if null impls then []
      else tokImplements ++ intercalate tokComma (map unparse impls),
    unparse block]

instance Unparse ClassStmt where
  unparse stmt = case stmt of
    CStmtVar pre a end -> IC.intercalUnparser id unparse pre ++
      intercalate tokComma (map unparse a) ++ unparse end
    CStmtConst a -> cStmtConstUnparser a
    CStmtFuncDef pre a -> unparsePre pre ++ unparse a
    CStmtAbstrFunc a -> unparse a
    CStmtCategory a -> tokCategory ++ a ++ tokSemi
    CStmtChildren a -> tokChildren ++ a ++ tokSemi
    CStmtAttribute a -> tokAttribute ++ a ++ tokSemi

cStmtConstUnparser :: (Unparse a) => [a] -> String
cStmtConstUnparser vars = tokConst ++
  intercalate tokComma (map unparse vars) ++ tokSemi

instance Unparse AbstrFunc where
  unparse (AbstrFunc pre ref name args ws end) = concat [unparsePre pre,
    tokFunction, maybe "" ((++ tokAmp) . unparse) ref, unparse name, tokLParen,
    either unparse (intercalate tokComma . map unparse) args, tokRParen,
    unparse ws, unparse end]

instance (Unparse a) => Unparse (VarEqVal a) where
  unparse (VarEqVal var w expr) = unparse var ++ w2With tokEquals w ++
    unparse expr

-- todo: the block form too?  does anyone use it?  declare is terrible anyway..
instance Unparse Declare where
  unparse (Declare (WSCap w1 (name, expr) w2) end) = concat [tokDeclare,
    unparse w1, tokLParen, unparse name, tokEquals, unparse expr, tokRParen,
    unparse w2, unparse end]

instance Unparse DoWhile where
  unparse (DoWhile block (WSCap w1 (WSCap w2 expr w3) w4) end) = concat [tokDo,
    unparse block, tokWhile, unparse w1, tokLParen, unparse w2, unparse expr,
    unparse w3, tokRParen, unparse w4, unparse end]

instance Unparse For where
  unparse (For (WSCap w1 (inits, conds, incrs) w2) block) = concat [
    tokFor, unparse w1, tokLParen,
    intercalate tokSemi $ map unparse [inits, conds, incrs],
    tokRParen, unparse w2, unparse block]

instance Unparse ForPart where
  unparse (ForPart e) = either unparse (intercalate tokComma . map unparse) e

instance Unparse Foreach where
  unparse (Foreach (WSCap w1 (expr, dubArrow) w2) block) = concat [tokForeach,
    unparse w1, tokLParen, unparse expr, tokAs, unparse dubArrow, tokRParen,
    unparse w2, unparse block]

instance Unparse Func where
  unparse (Func w1 ref name (WSCap w2 args w3) block) = concat [tokFunction,
    unparse w1, maybe [] ((tokAmp ++) . unparse) ref, name, unparse w2,
    tokLParen, argsUnparser args, tokRParen, unparse w3, unparse block]

argsUnparser :: (Unparse t, Unparse s) => Either t [s] -> String
argsUnparser = either unparse (intercalate tokComma . map unparse)

instance Unparse If where
  unparse (If isColon ifAndIfelses theElse) =
    tokIf ++ unparseIfBlock isColon theIf ++
    concatMap doIfelse ifelses ++
    maybe [] (\ (w1And2, blockOrStmt) -> w2With tokElse w1And2 ++
      colonUnparseBlockOrStmt isColon blockOrStmt) theElse ++
    if isColon then tokEndif else ""
    where
    (theIf, ifelses) = IC.breakStart ifAndIfelses
    doElsery Nothing = tokElseif
    doElsery (Just ws) = tokElse ++ unparse ws ++ tokIf
    doIfelse ((ws, elsery), ifBlock) =
      unparse ws ++ doElsery elsery ++ unparseIfBlock isColon ifBlock
    mbColon = if isColon then tokColon else ""

colonUnparseBlockOrStmt :: Bool -> BlockOrStmt -> String
colonUnparseBlockOrStmt isColon (Right (Block body)) = if isColon
  then tokColon ++ unparse body
  else unparse (Block body)
colonUnparseBlockOrStmt isColon (Left stmt) = if isColon
  -- We could just unparse the statement (which should be a one-statement
  -- block).  But it's probably better to yell on this invariant violation.
  then error "Colon notation should only use blocks."
  else unparse stmt

unparseIfBlock :: Bool -> IfBlock -> String
unparseIfBlock isColon (IfBlock (WSCap w1 expr w2) blockOrStmt) =
  concat [unparse w1, tokLParen, unparse expr, tokRParen, unparse w2] ++
  colonUnparseBlockOrStmt isColon blockOrStmt

instance Unparse Interface where
  unparse (Interface name extends block) = concat [tokInterface, unparse name,
    if null extends then []
      else tokExtends ++ intercalate tokComma (map unparse extends),
    unparse block]

instance Unparse IfaceStmt where
  unparse (IfaceConst vars) = cStmtConstUnparser vars
  unparse (IfaceFunc a) = unparse a

instance Unparse Namespace where
    unparse (Namespace n) = n

instance Unparse Use where
  unparse (Use n) = n

instance Unparse Switch where
  unparse (Switch (WSCap w1 expr w2) w3 cases) = concat [tokSwitch, unparse w1,
    tokLParen, unparse expr, tokRParen, unparse w2, tokLBrace, unparse w3,
    unparse cases, tokRBrace]

instance Unparse Case where
  unparse (Case expr stmtList) =
    either ((tokDefault ++) . unparse) ((tokCase ++) . unparse) expr ++
    tokColon ++ unparse stmtList

instance Unparse Catch where
  unparse (Catch (WSCap w1 (const, expr) w2) w3 block) = concat [tokCatch,
    unparse w1, tokLParen, unparse const, unparse expr,
    unparse w2, tokRParen, unparse w3, unparse block]

instance Unparse While where
  unparse (While (WSCap w1 expr w2) block) = concat [tokWhile, unparse w1,
    tokLParen, unparse expr, tokRParen, unparse w2, unparse block]
