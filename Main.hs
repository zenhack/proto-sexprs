import Data.ProtoSExprs

import Text.ParserCombinators.Parsec (runParser)

data LangStmt
    = Loop [LangStmt]
    | Assign String LangExpr
    deriving(Show)

data LangExpr
    = LangString String
    | LangBinOp String LangExpr LangExpr
    deriving(Show)


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
    print $ parseStmt <$> runParser pExpr () "" contents
