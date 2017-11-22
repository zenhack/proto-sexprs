{-# LANGUAGE DeriveGeneric #-}
import Data.ProtoSExprs
import GHC.Generics (Generic(..))

import Text.ParserCombinators.Parsec (runParser)

data LangStmt
    = Loop [LangStmt]
    | Assign String LangExpr
    deriving(Show, Generic)
instance ToExpr LangStmt


data LangExpr
    = LangString String
    | LangBinOp String LangExpr LangExpr
    deriving(Show, Generic)
instance ToExpr LangExpr



parseExpr :: Expr -> Maybe LangExpr
parseExpr (Str s) = Just (LangString s)
parseExpr (List [Atom op, lhs, rhs]) =
    LangBinOp op <$> parseExpr lhs <*> parseExpr rhs
parseExpr _ = Nothing

parseStmt :: Expr -> Maybe LangStmt
parseStmt (List (Atom "loop":stmts)) = Loop <$> mapM parseStmt stmts
parseStmt (List [Atom var, Atom ":=", ex]) = Assign var <$> parseExpr ex
parseStmt _ = Nothing

main = do
    contents <- getContents
    let exprs = runParser pFile () "" contents
    print exprs
    let stmts = map parseStmt <$> exprs
    print $ map parseStmt <$> exprs
    print $ toExpr $ Loop [ Assign "x" (LangString "hello")
                          , Assign "y" (LangBinOp "+" (LangString "f")
                                                      (LangString "x"))
                          ]
