module Calculator where

import Text.Read (readEither)

data Expr = Lit Int
    | Sub Expr Expr
    | Add Expr Expr
    | Mul Expr Expr
    | Div Expr Expr

eval :: Expr -> Int
eval expr =
    case expr of
        Lit num -> num
        Add arg1 arg2 -> (eval arg1) + (eval arg2)
        Sub arg1 arg2 -> (eval arg1) - (eval arg2)
        Mul arg1 arg2 -> (eval arg1) * (eval arg2)
        Div arg1 arg2 -> (eval arg1) `div` (eval arg2)

parse :: String -> Either String Expr
parse str =
    case parse' (words str) of  -- makes a list of words and passes to parse
        Left err -> Left err
        Right (e, []) -> Right e
        Right (_, rest) -> Left $ "Found extra tokens: " <> (unwords rest)

parse' :: [String] -> Either String (Expr, [String])
parse' [] = Left "unexpected end of expression"
parse' (token:rest) = -- Iterate each word
    case token of
        "+" -> parseBinary Add rest
        "*" -> parseBinary Mul rest
        "-" -> parseBinary Sub rest
        "/" -> parseBinary Div rest
        lit -> -- This should be a number
            case readEither lit of -- Tries to convert string to number
                Left err -> Left err
                Right lit' -> Right (Lit lit', rest)

parseBinary :: (Expr -> Expr -> Expr) -> [String] -> Either String (Expr, [String])
parseBinary exprConstructor args =
    case parse' args of -- focus on everything after Expr back in parse'
        Left err -> Left err
        Right (firstArg, rest') ->
            case parse' rest' of -- went fine, recursively parse again
                Left err -> Left err
                Right (secondArg, rest'') -> -- went fine, now we have the two evaluations we run our expr on
                    Right $ (exprConstructor firstArg secondArg, rest'')

run :: String -> String
run expr =
    case parse expr of
        Left err -> "Error: " <> err
        Right expr' ->
            let answer = show $ eval expr'
            in "The answer is: " <> answer
