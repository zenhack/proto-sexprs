

import Data.List (intersperse)

import Text.ParserCombinators.Parsec


data Ast
    = List [Ast]
    | Atom String
    | Str String


instance Show Ast where
    show (List as) = "[" ++ concat (intersperse " " (map show as)) ++ "]"
    show (Atom a) = a
    show (Str s) = concat
        [ "\""
        , concat $ map makeSafe s
        , "\""
        ]
      where
        makeSafe '"' = "\\\""
        makeSafe '\\' = "\\\\"
        makeSafe c = [c]

pAtom :: Parser Ast
pAtom = Atom <$> many1 (letter <|> digit <|> oneOf "+=-_*&^%$@!<>?/:\\")

pList :: Parser Ast
pList = List <$> between (char '[') (char ']') (pAst `sepBy` spaces)

pAst :: Parser Ast
pAst = pList <|> pStr <|> pAtom

pStr :: Parser Ast
pStr = Str <$> (char '"' *> (concat <$> many encoded) <* char '"') where
    encoded = (char '\\' >> escape) <|> strSafe
    strSafe = (:[]) <$> noneOf "\\\""
    escape = do
        c <- oneOf "abnrfvtxuU\\\""
        case c of
            'a' -> return "\a"
            'b' -> return "\b"
            'n' -> return "\n"
            'r' -> return "\r"
            'f' -> return "\f"
            'v' -> return "\v"
            't' -> return "\t"
            '\\' -> return "\\"
            '\"' -> return "\""
            'x' -> hexEsc 2
            'u' -> hexEsc 4
            'U' -> hexEsc 8
    hexEsc :: Int -> Parser String
    hexEsc n = do
        chrs <- count n hexDigit
        return $ (:[]) . toEnum . read $ "0x" ++ chrs


-- example:


data LangStmt
    = Loop [LangStmt]
    | Assign String LangExpr
    deriving(Show)

data LangExpr
    = LangString String
    | LangBinOp String LangExpr LangExpr
    deriving(Show)


parseExpr :: Ast -> Maybe LangExpr
parseExpr (Str s) = Just (LangString s)
parseExpr (List [Atom op, lhs, rhs]) =
    LangBinOp op <$> parseExpr lhs <*> parseExpr rhs
parseExpr _ = Nothing

parseStmt :: Ast -> Maybe LangStmt
parseStmt (List (Atom "loop":stmts)) = Loop <$> mapM parseStmt stmts
parseStmt (List [Atom var, Atom ":=", ex]) = Assign var <$> parseExpr ex
parseStmt _ = Nothing

main = do
    contents <- getContents
    print $ parseStmt <$> runParser pAst () "" contents
