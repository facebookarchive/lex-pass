{-# LANGUAGE DeriveDataTypeable, TemplateHaskell, FlexibleInstances,
             FlexibleContexts, OverlappingInstances #-}


module Lang.Php.Ast.ExprParse (LVal(..), RVal(..)) where

import Control.Monad.Identity
import Lang.Php.Ast.ArgList
import Lang.Php.Ast.Common
import Lang.Php.Ast.ExprTypes
import Lang.Php.Ast.Lex
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

instance Parse (Var, WS) where
  parse = tokDollarP >> (undyn <|> dyn) where
    undyn = do
      i <- genIdentifierParser
      -- try is here unless we combine processing for [expr] vs []
      (inds, ws) <- IC.breakEnd <$> IC.intercalParser parse (try $
        (tokLBracketP >> (,) True <$> parse <* tokRBracketP) <|>
        (tokLBraceP >> (,) False <$> parse <* tokRBraceP))
      return (Var i inds, ws)
    dyn = do
      ws <- parse
      first (VarDyn ws) <$> parse <|> first (VarDynExpr ws) <$> liftM2 (,)
        (tokLBraceP >> parse <* tokRBraceP) parse

parseABPairsUntilAOrC :: Parser a -> Parser b -> Parser c ->
  Parser ([(a, b)], Either a c)
parseABPairsUntilAOrC a b c = (,) [] . Right <$> c <|> do
  aR <- a
  (b >>= \ bR -> first ((aR, bR):) <$> parseABPairsUntilAOrC a b c) <|>
    return ([], Left aR)

dynConstOrConstParser :: Parser (Either DynConst Const, WS)
dynConstOrConstParser = do
  (statics, cOrD) <-
    first (map (\ ((a, b), c) -> (a, (b, c)))) <$>
    parseABPairsUntilAOrC (liftM2 (,) identifierParser parse)
    (tokDubColonP >> parse) parse
  return $ case cOrD of
    Left c -> first (Right . Const statics) c
    Right d -> first (Left . DynConst statics) d

exprOrLValParser :: Parser (Either Expr LVal, WS)
exprOrLValParser = try (first Left <$> parse) <|> first Right <$> parse

instance Parse (Val, WS) where
  parse = listVal <|> otherVal where
    listVal = tokListP >> liftM2 (,)
      (ValLOnlyVal <$> liftM2 LOnlyValList parse (mbArgListParser parse))
      parse
    otherVal = do
      (dOrC, ws) <- dynConstOrConstParser
      valExtend =<< case dOrC of
        Left d -> return (ValLRVal $ LRValVar d, ws)
        Right c -> (first ValROnlyVal <$>) $
          liftM2 (,) (ROnlyValFunc (Right c) ws <$> argListParser exprOrLValParser) parse
          <|> return (ROnlyValConst c, ws)

firstM :: (Monad m) => (a -> m b) -> (a, c) -> m (b, c)
firstM = runKleisli . first . Kleisli

instance Parse (LVal, WS) where
  parse = firstM f =<< parse where
    f r = case r of
      ValLOnlyVal v -> return $ LValLOnlyVal v
      ValROnlyVal _ -> fail "Expecting an LVal but found an ROnlyVal."
      ValLRVal v -> return $ LValLRVal v

instance Parse (RVal, WS) where
  parse = firstM f =<< parse where
    f r = case r of
      ValLOnlyVal _ -> fail "Expecting an RVal but found an LOnlyVal."
      ValROnlyVal v -> return $ RValROnlyVal v
      ValLRVal v -> return $ RValLRVal v

instance Parse (LRVal, WS) where
  parse = firstM f =<< parse where
    f r = case r of
      ValLOnlyVal _ -> fail "Expecting an LRVal but found an LOnlyVal."
      ValROnlyVal _ -> fail "Expecting an LRVal but found an ROnlyVal."
      ValLRVal v -> return v

-- val extending is like this:
-- L --member,index,append--> L
-- R --member--> LR
-- LR --member,index--> LR
-- LR --func--> R
-- LR --append--> L
valExtend :: (Val, WS) -> Parser (Val, WS)
valExtend v@(state, ws) = case state of
  ValLOnlyVal a ->
    do
      ws2 <- tokArrowP >> parse
      (memb, wsEnd) <- parse
      valExtend (ValLOnlyVal $ LOnlyValMemb a (ws, ws2) memb, wsEnd)
    <|> valExtendIndApp (LValLOnlyVal a) (ValLOnlyVal . LOnlyValInd a ws) ws
    <|> return v
  ValROnlyVal a -> valExtendMemb (RValROnlyVal a) ws
    <|> do
      ws2 <- tokLBracketP >> parse
      st <- ValLRVal . LRValInd (RValROnlyVal a) ws . capify ws2 <$>
        parse <* tokRBracketP
      valExtend =<< (,) st <$> parse
    <|> return v
  ValLRVal a ->
    do
      r <- liftM2 (,) (ValROnlyVal . ROnlyValFunc (Left a) ws <$>
        argListParser exprOrLValParser) parse
      valExtend r
    <|> valExtendIndApp (LValLRVal a) (ValLRVal . LRValInd (RValLRVal a) ws) ws
    <|> valExtendMemb (RValLRVal a) ws
    <|> return v

valExtendMemb :: RVal -> WS -> Parser (Val, WS)
valExtendMemb a ws = tokArrowP >> do
  ws2 <- parse
  (memb, wsEnd) <- parse
  valExtend (ValLRVal $ LRValMemb a (ws, ws2) memb, wsEnd)

instance Parse (Memb, WS) where
  parse =
    liftM2 (,) (
      (tokLBraceP >> MembExpr <$> parse <* tokRBraceP) <|>
      MembStr <$> genIdentifierParser) parse <|>
    first MembVar <$> parse

valExtendIndApp :: LVal -> (WSCap Expr -> Val) -> WS -> Parser (Val, WS)
valExtendIndApp lVal mkVal ws = tokLBracketP >> do
  ws2 <- parse
  st <-
    (tokRBracketP >>
      return (ValLOnlyVal $ LOnlyValAppend lVal (ws, ws2))) <|>
    mkVal . capify ws2 <$> (parse <* tokRBracketP)
  valExtend =<< (,) st <$> parse

varOrStringParser :: Parser (Either Var String, WS)
varOrStringParser = first Left <$> parse <|>
  liftM2 (,) (Right <$> identifierParser) parse

instance Parse (DynConst, WS) where
  parse = do
    statics <- many . liftM2 (,) identifierParser . liftM2 (,) parse $
      tokDubColonP >> parse
    first (DynConst statics) <$> parse

instance Parse (Const, WS) where
  parse = first (uncurry Const) . rePairLeft . first (map rePairRight) .
    IC.breakEnd <$> IC.intercalParser (liftM2 (,) identifierParser parse)
    (tokDubColonP >> parse)

lRValOrConstParser :: Parser (Either LRVal Const, WS)
lRValOrConstParser = do
  (v, w) <- parse
  case v of
    ValLRVal a -> return (Left a, w)
    ValROnlyVal (ROnlyValConst a) -> return (Right a, w)
    _ -> fail "Expected LRVal or Const but fould a different Val type."

-- Expr

instance Unparse Expr where
  unparse expr = case expr of
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

instance Parse (Expr, WS) where
  parse = buildExpressionParser exprParserTable simpleExprParser

simpleExprParser :: Parser (Expr, WS)
simpleExprParser = assignOrRValParser
  <|> do
    ws1 <- tokLParenP >> parse
    ambigCastParser ws1 <|> castOrParenParser ws1
  <|> do
    ws1 <- tokNewP >> parse
    (v, ws2) <- parse
    argsWSMb <- optionMaybe $ argListParser parse
    case argsWSMb of
      Just args -> (,) (ExprNew ws1 v $ Just (ws2, args)) <$> parse
      _ -> return (ExprNew ws1 v Nothing, ws2)
  <|> includeParser
  <|> do
    isExit <- return True <$> tokExitP <|> return False <$> tokDieP
    ws1 <- parse
    argMb <- optionMaybe $ exitListParser parse
    case argMb of
      Just arg -> (,) (ExprExit isExit $ Just (ws1, arg)) <$> parse
      _ -> return (ExprExit isExit Nothing, ws1)
  <|> do
    w <- tokAmpP >> parse
    first (ExprRef w . Right) <$> parse <|> do
      (e, wEnd) <- parse
      case e of
        ExprNew _ _ _ -> return (ExprRef w (Left e), wEnd)
        _ -> fail "Expecting a Val or ExprNew."
  <|> liftM2 (,) (
    ExprStrLit <$> parse <|>
    ExprNumLit <$> parse <|>
    ExprHereDoc <$> parse <|>
    (tokArrayP >> liftM2 ExprArray parse (arrListParser parse)) <|>
    funclike1Parser ExprEmpty tokEmptyP <|>
    funclike1Parser ExprEval tokEvalP <|>
    (tokIssetP >> liftM2 ExprIsset parse (issetListParser parse)) <|>
    ExprBackticks <$> backticksParser <|>
    ExprXml <$> parse
    ) parse

ambigCastParser :: WS -> Parser (Expr, WS)
ambigCastParser ws1 = try $ do
  i <- identsCI ["array", "unset"]
  ws2 <- parse
  ws3 <- tokRParenP >> parse
  first (ExprCast (WSCap ws1 i ws2) ws3) <$> parse

castOrParenParser :: WS -> Parser (Expr, WS)
castOrParenParser ws1 = do
  iMb <- optionMaybe $ identsCI ["int", "integer", "bool", "boolean",
    "float", "double", "real", "string", "binary", "object"]
  case iMb of
    Just i -> do
      ws2 <- parse
      ws3 <- tokRParenP >> parse
      first (ExprCast (WSCap ws1 i ws2) ws3) <$> parse
    _ -> liftM2 (,) (ExprParen . capify ws1 <$> parse <* tokRParenP) parse

assignOrRValParser :: Parser (Expr, WS)
assignOrRValParser = do
  (val, w) <- parse
  case val of
    ValLOnlyVal v -> assignCont (LValLOnlyVal v) w
    ValLRVal v -> assignCont (LValLRVal v) w <|>
      return (ExprRVal $ RValLRVal v, w)
    ValROnlyVal v -> return (ExprRVal $ RValROnlyVal v, w)

assignCont :: LVal -> WS -> Parser (Expr, WS)
assignCont l w1 = do
  o <- (tokEqualsP >> return Nothing) <|> Just <$> (
    (tokPlusByP   >> return BPlus) <|>
    (tokMinusByP  >> return BMinus) <|>
    (tokMulByP    >> return BMul) <|>
    (tokDivByP    >> return BDiv) <|>
    (tokConcatByP >> return BConcat) <|>
    (tokModByP    >> return BMod) <|>
    (tokBitAndByP >> return BBitAnd) <|>
    (tokBitOrByP  >> return BBitOr) <|>
    (tokXorByP    >> return BXor) <|>
    (tokShiftLByP >> return BShiftL) <|>
    (tokShiftRByP >> return BShiftR))
  w2 <- parse
  first (ExprAssign o l (w1, w2)) <$> parse

includeParser :: Parser (Expr, WS)
includeParser = try $ do
  i <- map toLower <$> genIdentifierParser
  f <- if i == tokRequireOnce then return $ ExprInclude Req Once else
    if i == tokIncludeOnce then return $ ExprInclude Inc Once else
    if i == tokRequire then return $ ExprInclude Req NotOnce else
    if i == tokInclude then return $ ExprInclude Inc NotOnce else
    fail "Expecting an include/require expression."
  ws <- parse
  first (f ws) <$> parse

instance Parse (DubArrowMb, WS) where
  parse = do
    (k, ws) <- parse
    vMb <- optionMaybe (tokDubArrowP >> liftM2 (,) parse parse)
    return $ case vMb of
      Just (ws2, (v, ws3)) -> (DubArrowMb (Just (k, (ws, ws2))) v, ws3)
      _ -> (DubArrowMb Nothing k, ws)

funclike1Parser :: (Parse (a, WS)) => (WS -> WSCap a -> b) -> Parser c ->
  Parser b
funclike1Parser constr tokP = liftM2 constr (tokP >> parse)
  (tokLParenP >> parse <* tokRParenP)

exprParserTable :: [[Oper (Expr, WS)]]
exprParserTable = [
  [Prefix eptClone],
  [Prefix eptPreIncr, Prefix eptPreDecr,
   Postfix eptPostIncr, Postfix eptPostDecr],
  [Postfix eptInstOf],
  [Prefix . preRep $ eptNot <|> eptBitNot <|> eptNegate <|> eptPos <|>
    eptSuppress],
  ial [eptMul, eptDiv, eptMod],
  ial [eptPlus, eptMinus, eptConcat],
  ial [eptShiftL, eptShiftR],
  ian [eptLT, eptLE, eptGT, eptGE, eptNEOld],
  ian [eptEQ, eptNE, eptID, eptNI],
  ial [eptBitAnd],
  ial [eptXor],
  ial [eptBitOr],
  [Prefix eptPrint],
  ial [eptAnd],
  ial [eptOr],
  [Postfix eptTernaryIf],
  ial [eptAndWd],
  ial [eptXorWd],
  ial [eptOrWd],
  [Postfix eptIndex]]

preRep, postRep :: Parser (a -> a) -> Parser (a -> a)
preRep p = (p >>= \ f -> (f .) <$> preRep p) <|> return id
postRep p = (p >>= \ f -> (. f) <$> postRep p) <|> return id

ial :: [Parser (a -> a -> a)] -> [Oper a]
ial = map $ flip Infix AssocLeft
ian = map $ flip Infix AssocNone

eptClone = preOp PrClone tokCloneP
eptPreIncr = preOp PrIncr tokIncrP
eptPreDecr = preOp PrDecr tokDecrP
eptPostIncr = postOp PoIncr tokIncrP
eptPostDecr = postOp PoDecr tokDecrP

preOp :: PreOp -> Parser a -> Parser ((Expr, WS) -> (Expr, WS))
preOp o p = do
  ws1 <- p >> parse
  return . first $ ExprPreOp o ws1

postOp :: PostOp -> Parser a -> Parser ((Expr, WS) -> (Expr, WS))
postOp o p = do
  ws2 <- p >> parse
  return $ \ (e, ws1) -> (ExprPostOp o e ws1, ws2)

binOp :: BinOp -> Parser a -> Parser ((Expr, WS) -> (Expr, WS) -> (Expr, WS))
binOp o p = do
  ws2 <- p >> parse
  return $ \ (e1, ws1) (e2, ws3) -> (ExprBinOp o e1 (ws1, ws2) e2, ws3)

eptBitNot = preOp PrBitNot tokBitNotP
eptNegate = preOp PrNegate tokMinusP
eptPos    = preOp PrPos tokPlusP
eptSuppress = preOp PrSuppress tokAtP

eptInstOf = do
  tokInstanceofP
  ws2 <- parse
  (t, ws3) <- lRValOrConstParser
  return $ \ (e, ws1) -> (ExprInstOf e (ws1, ws2) t, ws3)

eptNot = preOp PrNot tokNotP

eptMul = binOp (BByable BMul) tokMulP
eptDiv = binOp (BByable BDiv) tokDivP
eptMod = binOp (BByable BMod) tokModP
eptPlus   = binOp (BByable BPlus) tokPlusP
eptMinus  = binOp (BByable BMinus) tokMinusP
eptConcat = binOp (BByable BConcat) tokConcatP
eptShiftL = binOp (BByable BShiftL) tokShiftLP
eptShiftR = binOp (BByable BShiftR) tokShiftRP
eptLT     = binOp BLT     tokLTP
eptLE     = binOp BLE     tokLEP
eptGT     = binOp BGT     tokGTP
eptGE     = binOp BGE     tokGEP
eptNEOld  = binOp BNEOld  tokNEOldP
eptEQ     = binOp BEQ     tokEQP
eptNE     = binOp BNE     tokNEP
eptID     = binOp BID     tokIDP
eptNI     = binOp BNI     tokNIP

eptBitAnd = binOp (BByable BBitAnd) tokAmpP
eptXor    = binOp (BByable BXor) tokXorP
eptBitOr  = binOp (BByable BBitOr) tokBitOrP

eptPrint  = preOp PrPrint tokPrintP

eptAnd    = binOp BAnd    tokAndP
eptOr     = binOp BOr     tokOrP

eptTernaryIf :: Parser ((Expr, WS) -> (Expr, WS))
eptTernaryIf = do
  w2 <- tokQMarkP >> parse
  (e2, w3) <- parse
  w4 <- tokColonP >> parse
  (e3, w5) <- parse
  return $ \ (e1, w1) ->
    (ExprTernaryIf $ TernaryIf e1 (w1, w2) e2 (w3, w4) e3, w5)

eptAndWd = binOp BAndWd tokAndWdP
eptXorWd = binOp BXorWd tokXorWdP
eptOrWd  = binOp BOrWd  tokOrWdP

eptIndex :: Parser ((Expr, WS) -> (Expr, WS))
eptIndex = do
  e2 <- tokLBracketP >> parse
  w2 <- tokRBracketP >> parse
  return $ \ (e1, w1) -> (ExprIndex e1 w1 e2, w2)

instance Parse Xml where
  parse = tokLTP >> do
    tag <- many1 . oneOf $
      -- i thought _ wasn't allowed but i guess when marcel's away e will play
      [':', '-', '_'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
    attrs <- IC.intercalParser parse . liftM2 (,) xmlIdentifierParser $
      Just <$> try (liftM2 (,) (liftM2 (,) parse (tokEqualsP >> parse)) $
        (tokLBraceP >> Right <$> parse <* tokRBraceP) <|>
        Left <$> parse) <|>
      return Nothing
    content <- (tokDivP >> tokGTP >> return Nothing) <|>
      Just <$> liftM2 (,)
        (tokGTP >> many (Right <$> try parse <|> Left <$> parse))
        (tokLTP >> tokDivP >> ((string tag >> return True) <|> return False))
        <* tokGTP
    return $ Xml tag attrs content

instance Parse XmlLitOrExpr where
  parse = (tokLBraceP >> XmlExpr <$> parse <* tokRBraceP) <|>
    XmlLit <$> many1 (satisfy (`notElem` "<{"))

