{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lang.Php.Ast.StmtParse where

import Control.Monad.Identity
import Text.ParserCombinators.Parsec.Expr

import qualified Data.Intercal as IC
import Lang.Php.Ast.ArgList
import Lang.Php.Ast.Common
import Lang.Php.Ast.Lex
import Lang.Php.Ast.StmtTypes

-- Val

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
    parseABPairsUntilAOrC (liftM2 (,) (tokStaticP <|> identifierParser) parse)
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
          liftM2 (,)
            (ROnlyValFunc (Right c) ws <$> argListParser exprOrLValParser)
            parse
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

-- | val extending works like this:
-- - L --member,index,append--> L
-- - R --member--> LR
-- - LR --member,index--> LR
-- - LR --func--> R
-- - LR --append--> L
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
valExtendMemb a ws = (tokArrowP >> do
  ws2 <- parse
  (memb, wsEnd) <- parse
  valExtend (ValLRVal $ LRValMemb a (ws, ws2) memb, wsEnd))
		<|> (tokDubColonP >> do
		  ws2 <- parse
		  (memb, wsEnd) <- parse
		  valExtend (ValLRVal $ LRValStaMemb a (ws, ws2) memb, wsEnd))

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
    ExprAnonFunc <$> parse <|>
    (tokArrayP >> liftM2 ExprArray parse (arrListParser parse)) <|>
    funclike1Parser ExprEmpty tokEmptyP <|>
    funclike1Parser ExprEval tokEvalP <|>
    (tokIssetP >> liftM2 ExprIsset parse (reqArgListParser parse)) <|>
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
  [Postfix eptIndex],
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
  ial [eptOrWd]]

preRep, postRep :: Parser (a -> a) -> Parser (a -> a)
preRep p = (p >>= \ f -> (f .) <$> preRep p) <|> return id
postRep p = (p >>= \ f -> (. f) <$> postRep p) <|> return id

ial, ian :: [Parser (a -> a -> a)] -> [Oper a]
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
  (e2, w3) <- maybe (Nothing, []) (first Just) <$> parse
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

instance Parse (FuncArg, WS) where
  parse = do
    t <- optionMaybe
      (first Just <$> parse <|> (tokArrayP >> (,) Nothing <$> parse))
    ref <- optionMaybe (tokAmpP >> parse)
    first (FuncArg t ref) <$> parse

instance Parse AnonFuncUse where
  parse = tokUseP >>
    AnonFuncUse <$> wsCapParser (reqArgListParser parse)

-- We parse functions in two parts to disambiguate functions and anonymous
-- functions at top-level without using (try).
funcStartParser = tokFunctionP >> liftM2 (,)
  parse
  ((tokAmpP >> Just <$> parse) <|> return Nothing)

anonFuncContParser (w, ampMb) = liftM3 (AnonFunc w ampMb)
  (wsCapParser $ argListParser parse)
  parse
  parse

funcContParser (w, ampMb) = liftM3 (Func w ampMb)
  identifierParser
  (wsCapParser $ argListParser parse)
  parse

instance Parse AnonFunc where
  parse = funcStartParser >>= anonFuncContParser

-- Stmt

stmtListP :: Parser StmtList
stmtListP = liftM2 IC.unbreakStart parse parse

instance Parse (Stmt, WS) where
  parse =
    first StmtFor <$> parse <|>
    first StmtForeach <$> parse <|>
    first StmtIf <$> parse <|>
    first StmtWhile <$> parse <|>
    tryParser <|>
    liftM2 (,) simpleStmtParser parse

simpleStmtParser :: Parser Stmt
simpleStmtParser =
  StmtBlock <$> parse <|>
  breaklikeParser StmtBreak tokBreakP <|>
  StmtClass <$> parse <|>
  breaklikeParser StmtContinue tokContinueP <|>
  StmtDeclare <$> parse <|>
  StmtDoWhile <$> parse <|>
  liftM2 StmtEcho (tokEchoP >> sepBy1 parse tokCommaP) parse <|>
  (try $ liftM2 StmtStatic (tokStaticP >> sepBy1 parse tokCommaP) parse) <|>
  funcParser <|>
  stmtExprParser <|>
  liftM2 StmtGlobal (tokGlobalP >> sepBy1 parse tokCommaP) parse <|>
  liftM2 StmtNamespace (tokNamespaceP >> parse) parse <|>
  liftM2 StmtUse (tokUseP >> parse) parse <|>
  StmtInterface <$> parse <|>
  StmtNothing <$> parse <|>
  liftM3 StmtReturn (tokReturnP >> parse) (optionMaybe parse) parse <|>
  StmtSwitch <$> parse <|>
  liftM2 StmtThrow (tokThrowP >> parse) parse <|>
  liftM2 StmtUnset
    (tokUnsetP >> wsCapParser (reqArgListParser parse))
    parse

stmtExprParser :: Parser Stmt
stmtExprParser = stmtExprContParser parse

stmtExprContParser :: Parser (Expr, WS) -> Parser Stmt
stmtExprContParser p = liftM2 (uncurry StmtExpr) p parse

ifCondP :: Parser (WSCap2 Expr)
ifCondP = wsCapParser $ tokLParenP >> parse <* tokRParenP

instance Parse (If, WS) where
  parse = tokIfP >> do
    (isColon, ifBlockAndW) <- ifBlockP
    ifRestP isColon $ IC.Interend ifBlockAndW

-- | Parse the first conditional and block of an "if" control structure.
-- Returns:
-- - True iff the control structure uses the alternative colon-based
-- syntax
-- - the conditional and block
-- - any immediately trailing whitespace
ifBlockP :: Parser (Bool, (IfBlock, WS))
ifBlockP = do
  cond <- ifCondP
  let
    colonIf = do
      body <- stmtListP
      return (True, (IfBlock cond (Right $ Block body), []))
    normalIf = do
      ifBlockAndW <- first (IfBlock cond) <$> parse
      return (False, ifBlockAndW)
  (tokColonP >> colonIf) <|> normalIf

instance Parse (IfBlock, WS) where
  parse = do
    cond <- ifCondP
    first (IfBlock cond) <$> parse

-- | Parse the entire remainder of an "if" control structure given
-- is-it-colon-syntax and the first conditional-and-block.
ifRestP :: Bool -> IC.Intercal (IfBlock, WS) (Maybe WS) -> Parser (If, WS)
ifRestP isColon soFar =
  elseifContP isColon soFar <|>
  elseContP isColon soFar <|>
  do
    w' <- if isColon then tokEndifP >> parse else return []
    return (If isColon soFar' Nothing, w ++ w')
  where
  (soFar', w) = ifReconstr soFar

-- | Parse a conditional-and-block ensuring that its colon-syntax-or-not
-- matches the rest of the "if" control structure.
ifBlockPCheck :: Bool -> Parser (IfBlock, WS)
ifBlockPCheck isColon = do
  (isColon', ifBlockAndW) <- ifBlockP
  when (isColon /= isColon') $
    fail "You can't mix colon notation in one if block."
  return ifBlockAndW

-- | Parse the rest of an "if" control structure where the next token is
-- "elseif".
elseifContP :: Bool -> IC.Intercal (IfBlock, WS) (Maybe WS) -> Parser (If, WS)
elseifContP isColon soFar = tokElseifP >> do
  ifBlockAndW <- ifBlockPCheck isColon
  ifRestP isColon $ (\ x -> IC.append Nothing x soFar) ifBlockAndW

-- | Parse the rest of an "if" control structure where the next token is
-- "else".
elseContP :: Bool -> IC.Intercal (IfBlock, WS) (Maybe WS) -> Parser (If, WS)
elseContP isColon soFar = tokElseP >> do
  w <- parse
  elseIfContP isColon soFar w <|> elseEndP isColon soFar w

-- | Parse the rest of an "if" control structure where we've just seen
-- "else"+WS and will now see "if".
elseIfContP :: Bool -> IC.Intercal (IfBlock, WS) (Maybe WS) -> WS ->
  Parser (If, WS)
elseIfContP isColon soFar w = tokIfP >> do
  ifBlockAndW <- ifBlockPCheck isColon
  ifRestP isColon $ (\ x -> IC.append (Just w) x soFar) ifBlockAndW

-- | Parse the rest of an "if" control structure where we've just seen
-- "else"+WS and now there is only the final block.
elseEndP :: Bool -> IC.Intercal (IfBlock, WS) (Maybe WS) -> WS ->
  Parser (If, WS)
elseEndP True soFar w2 = do
  let (soFar', w1) = ifReconstr soFar
  block <- tokColonP >> Right . Block <$> stmtListP
  w3 <- tokEndifP >> parse
  return (If True soFar' $ Just ((w1, w2), block), w3)
elseEndP False soFar w2 = do
  let (soFar', w1) = ifReconstr soFar
  (block, wEnd) <- parse
  return (If False soFar' $ Just ((w1, w2), block), wEnd)

-- | Regroup a parsed "if" control structure to group WS together.
ifReconstr :: IC.Intercal (IfBlock, WS) (Maybe WS) ->
  (IC.Intercal IfBlock (WS, Maybe WS), WS)
ifReconstr a = (IC.unbreakEnd (map rePairRight main) ifBlockLast, w) where
  (main, (ifBlockLast, w)) = IC.breakEnd a

tryParser :: Parser (Stmt, WS)
tryParser = tokTryP >> do
  block <- parse
  first (StmtTry block) <$> intercalParserW parse

intercalParserW :: Parser a -> Parser (IC.Intercal a WS, WS)
intercalParserW a =
  (\ (aInit, (aLast, w)) -> (IC.unbreakEnd aInit aLast, w)) . unsnoc <$>
    many (liftM2 (,) a parse)

instance Parse Catch where
  parse = tokCatchP >> liftM3 Catch
    (liftM2 capify parse
      (tokLParenP >> liftM2 (curry rePairLeft) parse parse <* tokRParenP))
    parse
    parse

breaklikeParser :: (Maybe (WS, Expr) -> WS -> StmtEnd -> t) -> Parser b ->
  Parser t
breaklikeParser constr p = p >> do
  w1 <- parse
  eMb <- optionMaybe parse
  let
    (eMb', w) = case eMb of
      Just (e, w2) -> (Just (w1, e), w2)
      _ -> (Nothing, w1)
  constr eMb' w <$> parse

instance Parse Class where
  parse = liftM5 Class
    (many (liftM2 (,) (tokAbstractP <|> tokFinalP) parse))
    (tokClassP >> wsCapParser identifierParser)
    (optionMaybe $ tokExtendsP >> parse)
    ((tokImplementsP >> sepBy1 parse tokCommaP) <|> return [])
    parse

instance Parse ClassStmt where
  parse = classConstParser CStmtConst <|>
    xhpClassAttrParser <|>
    (tokChildrenP >> CStmtChildren <$> thruSemiParser) <|>
    (tokCategoryP >> CStmtCategory <$> thruSemiParser) <|>
    do
      r <- funcOrVarTypeToksP
      case r of
        (True, pre) -> classAbstrFuncParser CStmtAbstrFunc pre
        (False, []) -> classFuncParser []
        (False, pre) -> classFuncParser pre <|> classVarsParser pre

thruSemiParser :: Parser String
thruSemiParser = many (satisfy (/= ';')) <* tokSemiP

xhpClassAttrParser :: Parser ClassStmt
xhpClassAttrParser = tokAttributeP >> CStmtAttribute . concat <$> many (
  unparse <$> (parse :: Parser StrLit) <|>
  unparse <$> (parse :: Parser NumLit) <|>
  unparse <$> (parse :: Parser WSElem) <|>
  genIdentifierParser <|> tokAtP <|> tokMinusP <|>
  tokEqualsP <|> tokCommaP <|> tokLBraceP <|> tokRBraceP) <* tokSemiP

funcOrVarTypeToksP :: Parser (Bool, [(String, WS)])
funcOrVarTypeToksP = first or . unzip . map rePairRight <$> many (liftM2 (,) (
  (,) False <$> (tokProtectedP <|> tokPrivateP <|> tokPublicP <|>
    tokStaticP <|> tokVarP <|> tokFinalP) <|>
  (,) True <$> tokAbstractP) parse)

classConstParser :: ([WSCap (VarEqVal Const)] -> c) -> Parser c
classConstParser constr = tokConstP >>
  constr <$> sepBy1 parse tokCommaP <* tokSemiP

instance (Parse (a, WS)) => Parse (VarEqVal a, WS) where
  parse = do
    (var, w1) <- parse
    w2 <- tokEqualsP >> parse
    (val, w3) <- parse
    return (VarEqVal var (w1, w2) val, w3)

instance Parse (VarMbVal, WS) where
  parse = do
    (var, w1) <- parse
    liftM2 (\ w2 (val, w3) -> (VarMbVal var $ Just ((w1, w2), val), w3))
      (tokEqualsP >> parse) parse <|>
      return (VarMbVal var Nothing, w1)

classFuncParser :: [(String, WS)] -> Parser ClassStmt
classFuncParser pre = CStmtFuncDef pre <$> parse

classAbstrFuncParser :: (AbstrFunc -> c) -> [(String, WS)] -> Parser c
classAbstrFuncParser constr pre = constr <$> liftM5 (AbstrFunc pre)
  (tokFunctionP >> optionMaybe (try $ parse <* tokAmpP))
  parse
  (argListParser parse)
  parse
  parse

unsnoc :: [a] -> ([a], a)
unsnoc = first reverse . swap . uncons . reverse

classVarsParser :: [(String, WS)] -> Parser ClassStmt
classVarsParser pre = let (preInit, (s, w)) = unsnoc pre in
  liftM2 (CStmtVar (IC.unbreakEnd preInit s))
    (liftM2 (:) (capify w <$> parse)
      (many (tokCommaP >> parse)))
    parse

instance Parse Declare where
  parse = tokDeclareP >> liftM2 Declare
    (wsCapParser $
      tokLParenP >> liftM2 (,) parse (tokEqualsP >> parse) <* tokRParenP)
    parse

instance Parse DoWhile where
  parse = liftM3 DoWhile
    (tokDoP >> parse)
    (tokWhileP >> wsCapParser (tokLParenP >> parse <* tokRParenP))
    parse

instance (Parse (a, WS), Parse (b, WS)) => Parse (Either a b, WS) where
  parse = first Right <$> parse <|> first Left <$> parse

instance Parse (For, WS) where
  parse = tokForP >> do
    h <- wsCapParser $ tokLParenP >>
      liftM3 (,,) parse (tokSemiP >> parse <* tokSemiP) parse
      <* tokRParenP
    first (For h) <$> parse

instance Parse ForPart where
  parse = do
    w1 <- parse
    forPartExpry w1 <|> return (ForPart $ Left w1)

forPartExpry :: WS -> Parser ForPart
forPartExpry w1 = ForPart . Right <$>
  liftM2 (:) (capify w1 <$> parse) (many $ tokCommaP >> parse)

instance Parse (Foreach, WS) where
  parse = tokForeachP >> do
    h <- wsCapParser $
      tokLParenP >> liftM2 (,) parse (tokAsP >> parse) <* tokRParenP
    first (Foreach h) <$> parse

funcParser :: Parser Stmt
funcParser = do
  start <- funcStartParser
  StmtFuncDef <$> funcContParser start <|>
    -- We should actually implement something here like:
    --   exprContParser :: (Expr, WS) -> Parser (Expr, WS)
    -- since weird things like
    --   function (){} + 4;
    -- are grammatical even at top-level
    -- (just generates a warning a.k.a. a PHP "notice").
    -- Instead we use the less general stmtExprContParser,
    -- because such weird things are crazy anyway and this is easy for now.
    -- I believe there's actually no use to ever having a statement
    -- start-with/be an anon func anyway.  But we'll play along,
    -- allowing a statement to be an anon func at least.
    stmtExprContParser (toWsParser $
      ExprAnonFunc <$> anonFuncContParser start)

instance Parse Func where
  parse = funcStartParser >>= funcContParser

instance Parse Interface where
  parse = tokInterfaceP >> liftM3 Interface
    parse
    ((tokExtendsP >> sepBy1 parse tokCommaP) <|> return [])
    parse

instance Parse Namespace where
    parse = do
	n <- identifierParser
	return $ Namespace n

instance Parse Use where
    parse = do
      n <- identifierParser
      return $ Use n

instance Parse IfaceStmt where
  parse =
    classConstParser IfaceConst <|>
    classAbstrFuncParser IfaceFunc =<< snd <$> funcOrVarTypeToksP

instance Parse Switch where
  parse = tokSwitchP >> liftM3 Switch
    (wsCapParser $ tokLParenP >> parse <* tokRParenP)
    (tokLBraceP >> parse)
    parse <* tokRBraceP

instance Parse Case where
  parse = liftM2 Case
    ((tokDefaultP >> Left <$> parse) <|> (tokCaseP >> Right <$> parse))
    ((tokColonP <|> tokSemiP) >> stmtListP)

instance Parse (While, WS) where
  parse = tokWhileP >> do
    e <- wsCapParser $ tokLParenP >> parse <* tokRParenP
    first (While e) <$> parse

instance Parse (a, WS) => Parse (Block a) where
  parse = tokLBraceP >> Block <$> liftM2 IC.unbreakStart parse parse <*
    tokRBraceP

instance Parse TopLevel where
  parse = do
    (gotChars, text) <- upToCharsOrEndParser (const True) '<' '?'
    echoOrTok <- if gotChars
      then fmap Just $
        (char '=' >> Left <$> liftM2 (,) parse parse) <|>
        Right <$> (identCI "php" <|> return "")
      else return Nothing
    return $ TopLevel text echoOrTok

instance Parse StmtEnd where
  parse = (tokSemiP >> return StmtEndSemi) <|>
    (tokClosePhpP >> StmtEndClose <$> parse)
