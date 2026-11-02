import Data.Char

--Abstract syntax tree

data Ast = App Ast Ast
            | Add Ast Ast
            | Sub Ast Ast
            | Mult Ast Ast
            | Div Ast Ast
            | Val Int
            | Var String
            | Lambda String Ast
            deriving (Show, Eq, Read)

--Tokeniser

tokenise :: String -> [String]
tokenise [] = []
tokenise (x:xs)
    | isSpace x = tokenise xs
    | isDigit x = let (numStr, rest) = span isDigit (x:xs)
        in numStr : tokenise rest
    | isAlpha x = let (alphStr, rest) = span isAlpha (x:xs)
        in alphStr : tokenise rest
    | x == '-' && not (null xs) && head xs == '>' = "->" : tokenise (tail xs)
    | otherwise = [x] : tokenise (xs)

--Parser

parse :: String -> Ast
parse str = let (a, xs) = parseExpr (tokenise str) in
    if null xs then a
    else error ("Feil ved" ++ (head xs))

parseExpr :: [String] -> (Ast, [String])
parseExpr tokens
    | null xs = (a, xs)
    | head(xs) == "+" =
        let (b, rest) = parseExpr(tail(xs)) in (Add a b, rest)
    | head(xs) == "-" = 
        let (b, rest) = parseExpr(tail(xs)) in (Sub a b, rest)
    | otherwise = (a, xs)
    where (a, xs) = parseExpr1 tokens

parseExpr1 :: [String] -> (Ast, [String])
parseExpr1 tokens
    | null xs = (a, xs)
    | head(xs) == "*" =
        let (b, rest) = parseExpr1(tail(xs)) in (Mult a b, rest)
    | head(xs) == ":" = 
        let (b, rest) = parseExpr1(tail(xs)) in (Div a b, rest)
    | otherwise = (a, xs)
    where (a, xs) = parseValue tokens

parseValue :: [String] -> (Ast, [String])
parseValue("(":xs) = let (a, ")":rs) = parseExpr xs in (a,rs)

parseValue("[":xs) = (App a b, rs') 
    where (a, rs)      = parseValue xs
          (b, "]":rs') = parseValue rs

parseValue("|":str:"->":xs) = ((Lambda str a), rs)
    where (a, rs) = parseExpr xs

parseValue(x:xs) | all isDigit x = (Val (read x), xs)
                 | all isAlpha x = (Var x, xs)


--Unification

typeVariables :: [String]
typeVariables = [ "t" ++ show n | n <- [0..]]





