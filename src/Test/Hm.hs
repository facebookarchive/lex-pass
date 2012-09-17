import Control.Applicative hiding (Const)
import Data.Tree
--import PrettyPrint
--import Text.ParserCombinators.Parsec
import Text.PrettyPrint.GenericPretty

import Lang.Php.Ast.Common
import Lang.Php.Ast.StmtTypes
import Lang.Php.Ast.StmtParse

import Lang.Php.Ast.ExprTypes
import Lang.Php.Ast.Lex
import Data.Intercal

{-
class ToTree a where
  toTree :: a -> Tree String

instance ToTree If where
  toTree (If a b c) =
-}

myIfBlock = IfBlock
                                       {ifBlockExpr = WSCap {wsCapPre = [],
                                                             wsCapMain = WSCap {wsCapPre = [],
                                                                                wsCapMain = ExprRVal (RValROnlyVal (ROnlyValConst (Const []
                                                                                                                                         "true"))),
                                                                                wsCapPost = []},
                                                             wsCapPost = []},
                                        ifBlockBlock = Right $ Block (Intercal [WS "\n"]
                                                                             (StmtExpr (ExprAssign Nothing
                                                                                                   (LValLRVal (LRValVar (DynConst []
                                                                                                                                  (Var "a"
                                                                                                                                       []))))
                                                                                                   ([],
                                                                                                    [])
                                                                                                   (ExprNumLit (NumLit "4")))
                                                                                       []
                                                                                       StmtEndSemi)
                                                                             (Interend [WS "\n"]))}

run =
  runParser ((parse :: Parser (If, WS)) <* eof) () "lol" $
    intercalate "\n" ["if(true):","$a=4;","endif"]

main = do
  either print pp run

{-
*Lang.Php.Ast.StmtParse> runParser ((parse :: Parser (If, WS)) <* eof) () "lol" $ intercalate "\n" ["if(true):","$a=4;","endif"]
                                                                                                       Right (If {ifAltColonSyntax = True, ifAndIfelses = Interend (IfBlock {ifBlockExpr = WSCap {wsCapPre = [], wsCapMain = WSCap {wsCapPre = [], wsCapMain = ExprRVal (RValROnlyVal (ROnlyValConst (Const [] "true"))), wsCapPost = []}, wsCapPost = []}, ifBlockBlock = Right (Block (Intercal [WS "\n"] (StmtExpr (ExprAssign Nothing (LValLRVal (LRValVar (DynConst [] (Var "a" [])))) ([],[]) (ExprNumLit (NumLit "4"))) [] StmtEndSemi) (Interend [WS "\n"])))}), ifElse = Nothing},[])
*Lang.Php.Ast.StmtParse> runParser (ifRestP True (IC.Interend (IfBlock {ifBlockExpr = WSCap {wsCapPre = [], wsCapMain = WSCap {wsCapPre = [], wsCapMain = ExprRVal (RValROnlyVal (ROnlyValConst (Const [] "true"))), wsCapPost = []}, wsCapPost = []}, ifBlockBlock = Right (Block (Intercal [WS "\n"] (StmtExpr (ExprAssign Nothing (LValLRVal (LRValVar (DynConst [] (Var "a" [])))) ([],[]) (ExprNumLit (NumLit "4"))) [] StmtEndSemi) (Interend [WS "\n"])))}), ifElse = Nothing}, []))) <* eof) () "lol" $ intercalate "\n" ["if(true):","$a=4;","endif"]

<interactive>:26:435: parse error on input `='
*Lang.Php.Ast.StmtParse> runParser (ifRestP True (IC.Interend (IfBlock {ifBlockExpr = WSCap {wsCapPre = [], wsCapMain = WSCap {wsCapPre = [], wsCapMain = ExprRVal (RValROnlyVal (ROnlyValConst (Const [] "true"))), wsCapPost = []}, wsCapPost = []}, ifBlockBlock = Right (Block (Intercal [WS "\n"] (StmtExpr (ExprAssign Nothing (LValLRVal (LRValVar (DynConst [] (Var "a" [])))) ([],[]) (ExprNumLit (NumLit "4"))) [] StmtEndSemi) (Interend [WS "\n"])))}), ifElse = Nothing}, [])) <* eof) () "lol" $ intercalate "\n" ["if(true):","$a=4;","endif"]

<interactive>:27:435: parse error on input `='
*Lang.Php.Ast.StmtParse>
Leaving GHCi.
-}
