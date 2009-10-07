{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.Php.Ast.Expr (LVal(..), RVal(..)) where

import Control.Monad.Identity
import Lang.Php.Ast.ArgList
import Lang.Php.Ast.Common
import Lang.Php.Ast.ExprTypes
import Lang.Php.Ast.Lex
import Text.ParserCombinators.Parsec.Expr
import qualified Data.Intercal as IC

-- Val

instance Unparse Var where
  unparse (Var s indexes) = tokDollar ++ s ++ concatMap (\ (ws, expr) ->
    unparse ws ++ tokLBracket ++ unparse expr ++ tokRBracket) indexes
  unparse (VarDyn ws var) = tokDollar ++ unparse ws ++ unparse var
  unparse (VarDynExpr ws expr) = unparse ws ++ tokLBrace ++ unparse expr ++
    tokRBrace

instance Unparse Const where
  unparse (Const statics s) = concatMap (\ (s, (ws1, ws2)) -> s ++
    unparse ws1 ++ tokDubColon ++ unparse ws2) statics ++ s

instance Unparse DynConst where
  unparse (DynConst statics var) = concatMap (\ (s, (ws1, ws2)) -> s ++
    unparse ws1 ++ tokDubColon ++ unparse ws2) statics ++ unparse var

instance Unparse LRVal where
  unparse (LRValVar a) = unparse a
  unparse (LRValMemb v (ws1, ws2) s) =
    unparse v ++ unparse ws1 ++ tokArrow ++ unparse ws2 ++ s

instance Unparse LOnlyVal where
  unparse (LOnlyValAppend v (ws1, ws2)) =
    unparse v ++ unparse ws1 ++ tokLBracket ++ unparse ws2 ++ tokRBracket
  unparse (LOnlyValInd v ws expr) =
    unparse v ++ unparse ws ++ tokLBracket ++ unparse expr ++ tokRBracket
  unparse (LOnlyValMemb v (ws1, ws2) s) =
    unparse v ++ unparse ws1 ++ tokArrow ++ unparse ws2 ++ s

instance Unparse ROnlyVal where
  unparse (ROnlyValConst a) = unparse a
  unparse (ROnlyValFunc v ws (Left w)) = unparse v ++ unparse ws ++ unparse w
  unparse (ROnlyValFunc v ws (Right args)) = unparse v ++ unparse ws ++
    intercalate tokComma (map unparse args)

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

instance ParseW Var where
  parseW = tokDollarP >> (undyn <|> dyn) where
    undyn = do
      i <- genIdentifierParser
      -- try is here unless we combine processing for [expr] vs []
      (inds, ws) <- IC.breakEnd <$> IC.intercalParser parse
        (try $ tokLBracketP >> liftM2 capify parse parseW <* tokRBracketP)
      return (Var i inds, ws)
    dyn = do
      ws <- parse
      first (VarDyn ws) <$> parseW <|> first (VarDynExpr ws) <$> liftM2 (,)
        (tokLBraceP >> liftM2 capify parse parseW <* tokRBraceP) parse

parseABPairsUntilAOrC :: Parsec String () a -> Parsec String () b ->
  Parsec String() c -> Parsec String () ([(a, b)], Either a c)
parseABPairsUntilAOrC a b c = (,) [] . Right <$> c <|> do
  aR <- a
  (b >>= \ bR -> first ((aR, bR):) <$> parseABPairsUntilAOrC a b c) <|>
    return ([], Left aR)

dynConstOrConstParser :: Parsec String () (Either DynConst Const, WS)
dynConstOrConstParser = do
  (statics, cOrD) <-
    first (map (\ ((a, b), c) -> (a, (b, c)))) <$>
    parseABPairsUntilAOrC (liftM2 (,) identifierParser parse)
    (tokDubColonP >> parse) parseW
  return $ case cOrD of
    Left c -> first (Right . Const statics) c
    Right d -> first (Left . DynConst statics) d

instance ParseW Val where
  parseW = listVal <|> otherVal where
    listVal = tokListP >> liftM2 (,)
      (ValLOnlyVal <$> liftM2 LOnlyValList parse (mbArgListParser parseW))
      parse
    otherVal = do
      (dOrC, ws) <- dynConstOrConstParser
      valExtend =<< case dOrC of
        Left d -> return (ValLRVal $ LRValVar d, ws)
        Right c -> (first ValROnlyVal <$>) $
          liftM2 (,) (ROnlyValFunc (Right c) ws <$> argListParser parseW) parse
          <|> return (ROnlyValConst c, ws)

firstM = runKleisli . first . Kleisli

instance ParseW LVal where
  parseW = firstM f =<< parseW where
    f r = case r of
      ValLOnlyVal v -> return $ LValLOnlyVal v
      ValROnlyVal _ -> fail "Expecting an LVal but found an ROnlyVal."
      ValLRVal v -> return $ LValLRVal v

instance ParseW RVal where
  parseW = firstM f =<< parseW where
    f r = case r of
      ValLOnlyVal _ -> fail "Expecting an RVal but found an LOnlyVal."
      ValROnlyVal v -> return $ RValROnlyVal v
      ValLRVal v -> return $ RValLRVal v

-- val extending is like this:
-- L --member,index,append--> L (L can never be used with - so -> is easy)
-- R --member--> LR (annoying to distinguish -> and -)
-- LR --member--> LR (annoying to distinguish -> and -)
-- LR --func--> R
-- LR --append--> L
valExtend :: (Val, WS) -> Parsec String () (Val, WS)
valExtend v@(state, ws) = case state of
  ValLOnlyVal a ->
    do
      tokArrowP
      st <- ValLOnlyVal <$>
        liftM2 (LOnlyValMemb a) (((,) ws) <$> parse) identifierParser
      wsEnd <- parse
      valExtend (st, wsEnd)
    <|> do
      ws1 <- tokLBracketP >> parse
      valExtend =<<
        (tokRBracketP >> ((,) (ValLOnlyVal $ LOnlyValAppend (LValLOnlyVal a)
        (ws, ws1))) <$> parse) <|>
        liftM2 (,) (ValLOnlyVal . LOnlyValInd a ws . capify ws1 <$> parseW)
          parse
    <|> return v
  ValROnlyVal a ->
    do
      try tokArrowP
      st <- ValLRVal <$> liftM2 (LRValMemb $ RValROnlyVal a)
        (((,) ws) <$> parse) identifierParser
      wsEnd <- parse
      valExtend (st, wsEnd)
    <|> return v
  ValLRVal a ->
    do
      r <- liftM2 (,) (ValROnlyVal . ROnlyValFunc (Left a) ws <$>
        argListParser parseW) parse
      valExtend r
    <|> do
      r <- tokLBracketP >> liftM2 (,)
        (ValLOnlyVal . LOnlyValAppend (LValLRVal a) . ((,) ws) <$> parse)
        (tokRBracketP >> parse)
      valExtend r
    <|> do
      try tokArrowP
      ws1 <- parse
      valExtend =<< liftM2 (,)
        (ValLRVal . LRValMemb (RValLRVal a) (ws, ws1) <$> identifierParser)
        parse
    <|> return v

instance ParseW DynConst where
  parseW = do
    statics <- many . liftM2 (,) identifierParser . liftM2 (,) parse $
      tokDubColonP >> parse
    first (DynConst statics) <$> parseW

instance ParseW Const where
  parseW = first (uncurry Const) . rePairLeft . first (map rePairRight) .
    IC.breakEnd <$> IC.intercalParser (liftM2 (,) identifierParser parse)
    (tokDubColonP >> parse) where
    rePairLeft (a, (b, c)) = ((a, b), c)
    rePairRight ((a, b), c) = (a, (b, c))

lRValOrConstParser = do
  (v, w) <- parseW
  case v of
    ValLRVal a -> return (Left a, w)
    ValROnlyVal (ROnlyValConst a) -> return (Right a, w)
    _ -> fail "Expected LRVal or Const but fould a different Val type."

-- Expr

instance Unparse Expr where
  unparse expr = case expr of
    ExprArray w elemsOrW -> unparse w ++ either unparse f elemsOrW where
      f (elems, wEnd) = intercalate tokComma .
        maybe id (flip (++) . (:[]) . unparse) wEnd $ map unparse elems
    ExprAssign notW v (w1, w2) e -> maybe "" ((tokNot ++) . unparse) notW ++
      unparse v ++ unparse w1 ++ tokEquals ++ unparse w2 ++ unparse e
    ExprBackticks a -> a
    ExprBinOp o e1 (w1, w2) e2 -> unparse e1 ++ unparse w1 ++ unparse o ++
      unparse w2 ++ unparse e2
    ExprCast t w e -> unparse t ++ unparse w ++ unparse e
    ExprEmpty w e -> tokEmpty ++ unparse w ++ tokLParen ++ unparse e ++
      tokRParen
    ExprEval w e -> tokEval ++ unparse w ++ tokLParen ++ unparse e ++
      tokRParen
    ExprExit isExit a -> (if isExit then tokExit else tokDie) ++
      maybe "" (\ (w, x) -> unparse w ++ either unparse unparse x) a
    ExprHereDoc a -> unparse a
    ExprInclude a b w e -> unparse a ++ unparse b ++ unparse w ++ unparse e
    ExprInstOf e (w1, w2) t -> unparse e ++ unparse w1 ++ tokInstanceof ++
      unparse t
    ExprIsset w vs -> tokIsset ++ unparse w ++ intercalate tokComma
      (map unparse vs)
    ExprNew w a argsMb -> tokNew ++ unparse w ++ unparse a ++ maybe ""
      (\ (wPre, args) -> unparse wPre ++ tokLParen ++ either unparse
        (intercalate tokComma . map unparse) args ++ tokRParen) argsMb
    ExprNumLit a -> unparse a
    ExprParen a -> tokLParen ++ unparse a ++ tokRParen
    ExprPreOp o w e -> unparse o ++ unparse w ++ unparse e
    ExprPreIncr t w v -> unparse t ++ unparse w ++ unparse v
    ExprPostIncr t v w -> unparse t ++ unparse v ++ unparse w
    ExprRef w v -> unparse tokAmp ++ unparse w ++ unparse v
    ExprRVal a -> unparse a
    ExprStrLit a -> unparse a
    ExprTernaryIf a -> unparse a

instance Unparse IncrOrDecr where
  unparse Incr = tokIncr
  unparse Decr = tokDecr

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
    BByable o -> unparse o ++ tokEquals

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

instance ParseW Expr where
  parseW = buildExpressionParser exprAfterByTable exprUpToByParser

assignEtcParser :: Parsec String () (Expr, WS)
assignEtcParser = do
  notMb <- optionMaybe (tokNotP >> parse)
  var <-

  liftM2 (\ a (b, c) -> (ExprPreOp PrNot a b, c)) (tokNotP >> parse) parseW
  <|> first ExprRVal <$> parseW

simpleExprParser :: Parsec String () (Expr, WS)
simpleExprParser = assignOrNotOrRValParser
  <|> first ExprStrLit <$> parseW
  <|> first ExprNumLit <$> parseW
  <|> do
    ws1 <- tokLParenP >> parse
    iMb <- optionMaybe $ identsCI ["int", "integer", "bool", "boolean",
      "float", "double", "real", "string", "binary", "array", "object",
      "unset"]
    case iMb of
      Just i -> do
        ws2 <- parse
        ws3 <- tokRParenP >> parse
        first (ExprCast (WSCap ws1 i ws2) ws3) <$> parseW
      _ -> liftM2 (,) (ExprParen . capify ws1 <$> parseW <* tokRParenP) parse
  <|> liftM2 (,) (
    (tokArrayP >> liftM2 ExprArray parse (arrListParser parseW)) <|>
    funclike1Parser ExprEmpty tokEmptyP <|>
    funclike1Parser ExprEval tokEvalP <|>
    (tokIssetP >> liftM2 ExprIsset parse (issetListParser parseW)) <|>
    ExprBackticks <$> backticksParser
    --(tokAtP >> ExprPreOp PrSuppress)
    ) parse
  <|> includeParser
  <|> do
    ws1 <- tokNewP >> parse
    (dOrC, ws2) <- dynConstOrConstParser
    argsWSMb <- optionMaybe $ argListParser parseW
    case argsWSMb of
      Just args -> (,) (ExprNew ws1 dOrC $ Just (ws2, args)) <$> parse
      _ -> return (ExprNew ws1 dOrC Nothing, ws2)
  <|> do
    isExit <- return True <$> tokExitP <|> return False <$> tokDieP
    ws1 <- parse
    argMb <- optionMaybe $ exitListParser parseW
    case argMb of
      Just arg -> (,) (ExprExit isExit $ Just (ws1, arg)) <$> parse
      _ -> return (ExprExit isExit Nothing, ws1)
  --[Prefix eptRef],
  {-
  ExprPreIncr
  ExprPostIncr
  -}

includeParser :: Parsec String () (Expr, WS)
includeParser = try $ do
  i <- map toLower <$> genIdentifierParser
  f <- if i == tokRequireOnce then return $ ExprInclude Req Once else
    if i == tokIncludeOnce then return $ ExprInclude Inc Once else
    if i == tokRequire then return $ ExprInclude Req NotOnce else
    if i == tokInclude then return $ ExprInclude Inc NotOnce else
    fail "Expecting an include/require expression."
  ws <- parse
  first (f ws) <$> parseW

instance ParseW DubArrowMb where
  parseW = do
    (k, ws) <- parseW
    vMb <- optionMaybe (tokDubArrowP >> liftM2 (,) parse parseW)
    return $ case vMb of
      Just (ws2, (v, ws3)) -> (DubArrowMb (Just (k, (ws, ws2))) v, ws3)
      _ -> (DubArrowMb Nothing k, ws)

funclike1Parser :: (ParseW a) => (WS -> WSCap a -> b) -> Parsec String () c ->
  Parsec String () b
funclike1Parser constr tokP = liftM2 constr (tokP >> parse) $
  liftM2 capify (tokLParenP >> parse) (parseW <* tokRParenP)

exprUpToByTable :: [[Operator String () Identity (Expr, WS)]]
exprUpToByTable = [
  [Prefix eptClone],
  {-
  [Prefix eptPreIncr, Prefix eptPreDecr,
   Postfix eptPostIncr, Postfix eptPostDecr],
   -}
  map Prefix [eptBitNot, eptNegate, eptPos, eptAt],
  [Postfix eptInstOf],
  [Prefix $ preRep eptNot],
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
  [Postfix eptTernaryIf]]

--eptRef    = preOp PrRef   tokRefP
--eptPreIncr  = wsAfter  ExprPreIncr  (try tokIncrP)
{-
eptPreDecr  = wsAfter  ExprPreDecr  tokDecr
eptPostIncr = wsBefore ExprPostIncr tokIncr
eptPostDecr = wsBefore ExprPostDecr tokDecr
-}

exprUpToByParser :: Parsec String () (Expr, WS)
exprUpToByParser = do
  r@(e1, ws1) <- p
  case e1 of
    ExprRVal
  oMb <- optionMaybe $ (tokEqualsP >> return Nothing) <|> Just <$> (
    (tokPlusByP  >> return BPlus) <|>
    (tokMinusBy  >> return BMinus) <|>
    (tokMulBy    >> return BMul) <|>
    (tokDivBy    >> return BDiv) <|>
    (tokConcatBy >> return BConcat) <|>
    (tokModBy    >> return BMod) <|>
    (tokBitAndBy >> return BBitAnd) <|>
    (tokBitOrBy  >> return BBitOr) <|>
    (tokBitXorBy >> return BBitXor) <|>
    (tokShiftLBy >> return BShiftL) <|>
    (tokShiftRBy >> return BShiftR))
  case oMb of
    Just o -> do
      ws2 <- parse
    Nothing -> return r


  (ws1, constr) <-

    (tokEqualsP >> undefined) <|>
    --second (const ExprAssign)   <$> tokEqualsP   <|>
    second (const ExprPlusBy)   <$> tokPlusByP   <|>
    second (const ExprMinusBy)  <$> tokMinusByP  <|>
    second (const ExprMulBy)    <$> tokMulByP    <|>
    second (const ExprDivBy)    <$> tokDivByP    <|>
    second (const ExprConcatBy) <$> tokConcatByP <|>
    second (const ExprModBy)    <$> tokModByP    <|>
    second (const ExprBitAndBy) <$> tokBitAndByP <|>
    second (const ExprBitOrBy)  <$> tokBitOrByP  <|>
    second (const ExprBitXorBy) <$> tokBitXorByP <|>
    second (const ExprShiftLBy) <$> tokShiftLByP <|>
    second (const ExprShiftRBy) <$> tokShiftRByP
  where p = buildExpressionParser exprUpToByTable simpleExprParser

exprAfterByTable :: [[Operator String () Identity (Expr, WS)]]
exprAfterByTable = [
  ial [eptAndWd],
  ial [eptXorWd],
  ial [eptOrWd]]

preRep, postRep :: Parsec String () (a -> a) -> Parsec String () (a -> a)
preRep p = (p >>= \ f -> (f .) <$> preRep p) <|> return id
postRep p = (p >>= \ f -> (. f) <$> postRep p) <|> return id

ial, ian :: [Parsec String () (a -> a -> a)] -> [Operator String () Identity a]
ial = map $ flip Infix AssocLeft
ian = map $ flip Infix AssocNone

eptClone = preOp PrClone tokCloneP

preOp :: PreOp -> Parsec String () a ->
  Parsec String () ((Expr, WS) -> (Expr, WS))
preOp o p = do
  ws1 <- p >> parse
  return . first $ ExprPreOp o ws1

binOp :: BinOp -> Parsec String () a ->
  Parsec String () ((Expr, WS) -> (Expr, WS) -> (Expr, WS))
binOp o p = do
  ws2 <- p >> parse
  return $ \ (e1, ws1) (e2, ws3) -> (ExprBinOp o e1 (ws1, ws2) e2, ws3)

eptBitNot = preOp PrBitNot tokBitNotP
eptNegate = preOp PrNegate tokMinusP
eptPos    = preOp PrPos tokPlusP
eptAt     = preOp PrSuppress tokAtP

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

eptTernaryIf :: Parsec String () ((Expr, WS) -> (Expr, WS))
eptTernaryIf = do
  w2 <- tokQMarkP >> parse
  (e2, w3) <- parseW
  w4 <- tokColonP >> parse
  (e3, w5) <- parseW
  return $ \ (e1, w1) ->
    (ExprTernaryIf $ TernaryIf e1 (w1, w2) e2 (w3, w4) e3, w5)

eptAndWd = binOp BAndWd tokAndWdP
eptXorWd = binOp BXorWd tokXorWdP
eptOrWd  = binOp BOrWd  tokOrWdP

