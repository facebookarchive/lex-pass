{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Lang.Php.Ast.Stmt where

import Lang.Php.Ast.Common

data Stmt =
  StmtExpr Expr WS StmtEnd
  -- todo
  deriving (Eq, Show, Typeable, Data)

instance Parse Stmt where
  parse = liftM3 StmtExpr parser parser parser

instance Unparse Stmt where
  unparse (StmtExpr a b c) = concat [unparse a, unparse b, unparse c]

$(derive makeBinary ''Stmt)

data StmtEnd = StmtEndSemi | StmtEndClose
  deriving (Eq, Show, Typeable, Data)

-- fixme: should these string literals be tokBlah's?
instance Parse StmtEnd where
  parse =
    (char ';' >> return StmtEndSemi) <|>
    (string "?>" >> return StmtEndClose)

instance Unparse StmtEnd where
  unparse StmtEndSemi = ";"
  unparse StmtEndClose = "?>"

$(derive makeBinary ''StmtEnd)

