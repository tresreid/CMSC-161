import Data.Char
import Data.List

data Op = Plus | Mult
    deriving (Show, Eq)

data ArithExpr = Number Integer
                | Operator Op ArithExpr ArithExpr
    deriving (Show, Eq)

testExpr1 = Operator Plus (Number 3) (Operator Plus (Number 1) (Number 2))
testExpr2 = Operator Plus (Number 3) (Operator Mult (Number 4) (Number 2))
testExpr3 = Operator Plus (Number 1) (Operator Plus (Operator Mult (Number 2) (Number 3)) (Operator Mult (Operator Mult (Number (-5)) (Number 3)) (Operator Plus (Number 4) (Number (-2)))))

eval :: ArithExpr -> Integer
eval (Number int) = int
eval (Operator Plus e1 e2) = eval e1 + eval e2
eval (Operator Mult e1 e2) = eval e1 * eval e2

testEvalExpr1 = eval (Number 7) == 7
testEvalExpr2 = eval (Operator Plus (Number 5) (Operator Mult (Number 1) (Number 2))) == 7
testEvalExpr3 = eval (Operator Plus (Number 0) (Operator Mult (Operator Plus (Number 2) (Number (-3))) (Operator Plus (Operator Plus (Number (-1)) (Number 2)) (Operator Mult (Number 4) (Number (-2)))))) == 7
testEvalExpr4 = eval (Operator Mult (Number (-2)) (Operator Mult (Number 7) (Operator Plus (Number 0) (Number (-5))))) == 70

removeWhiteSpace :: String -> String
removeWhiteSpace [] = []
removeWhiteSpace (x:xs)
    | isSpace x = removeWhiteSpace xs
    | otherwise = x : removeWhiteSpace xs

testRemoveWhiteSpace1 = removeWhiteSpace " 1  \n234 \t2 \t \n 222" == "12342222"
testRemoveWhiteSpace2 = removeWhiteSpace "yolo\nyolo\tyolo\nhello\thello" == "yoloyoloyolohellohello"

readTokens :: String -> [String]
readTokens xs = if xs == ""  -- Base case
                    then [] -- no more tokens
                    else token:(readTokens rest)
                    -- Otherwise, read a single token
                    -- from the start of the string and recurse
    where (token, rest) = readToken xs

isInt :: String -> Bool
isInt (x:xs) = all isDigit xs && ((x == '-') || isDigit x)

readToken :: String -> (String, String) -- First string is token
readToken (x:xs)
    -- Ignore spaces
    | isSpace x  = readToken xs
    -- Token is negative number
    | x == '-' && isDigit (head xs) = (x:(takeWhile isDigit xs),
                                            dropWhile isDigit xs) 
    -- Token is positive number
    | isDigit x = (takeWhile isDigit (x:xs),
                             dropWhile isDigit xs)
    -- Token is operator
    | x `elem` ['+','*'] = ([x], xs) -- Token is an operator
    | x == '(' = readBracketed (x:xs) -- Token is bracketed subexpression
    | otherwise = error ("Unrecognised token at head of " ++ (x:xs))

-- the first character in the string should be a left
-- parenthesis.  This function reads up until the matching
-- right parenthesis, and returns that string and the remainder
readBracketed :: String -> (String, String)
readBracketed ('(':xs) = findMatching 0 "" xs
    -- the first argument in findMatching keeps track of
    -- how many extra layers of parentheses we are inside
    where -- we found the matching parenthesis as the count is down to zero
          findMatching 0 matched (')':xs) = (matched, xs) 
          -- entering another layer of parentheses
          findMatching n matched ('(':xs) = findMatching (n + 1) (matched ++ "(") xs
          -- exited a layer of parentheses
          findMatching n matched (')':xs) = findMatching (n - 1) (matched ++ ")") xs
          -- non-parenthesis character
          findMatching n matched (x:xs) = findMatching n (matched ++ [x]) xs

testTokenize1 = readTokens "1024+13*-12" == ["1024","+","13","*","-12"]
testTokenize2 = readTokens "1+(1+-3)*(-5*2)" == ["1","+","1+-3","*","-5*2"]
testTokenize3 = readTokens "-1+(-1*2*(1+2))" == ["-1","+","-1*2*(1+2)"]

arithmetize :: [String] -> ArithExpr
arithmetize xs
    | length xs == 1 && isInt (head xs) = Number (read (head xs))
    | length xs == 1 = parseExpr (head xs)
    | "+" `elem` xs = Operator Plus (arithmetize plus1) (arithmetize (tail(plus2)))
    | "*" `elem` xs = Operator Mult (arithmetize times1) (arithmetize (tail(times2)))
    | otherwise = error ("Invalid tokens: " ++ (show xs))
    where (plus1,plus2) = break (=="+") xs
    	  (times1,times2) = break (=="*") xs

testArithmetize1 = arithmetize (readTokens "1+3*5*-3") == Operator Plus (Number 1) (Operator Mult (Number 3) (Operator Mult (Number 5) (Number (-3))))
testArithmetize2 = arithmetize (readTokens "1+(-3*5)") == Operator Plus (Number 1) (Operator Mult (Number (-3)) (Number 5))
testArithmetize3 = arithmetize (readTokens "-1+(3*(3+-4)+1)+1") == Operator Plus (Number (-1)) (Operator Plus (Operator Plus (Operator Mult (Number 3) (Operator Plus (Number 3) (Number (-4)))) (Number 1)) (Number 1))
testArithmetize4 = arithmetize (readTokens "0+0*0+((0*0)*(0+0))+(0)") == Operator Plus (Number 0) (Operator Plus (Operator Mult (Number 0) (Number 0)) (Operator Plus (Operator Mult (Operator Mult (Number 0) (Number 0)) (Operator Plus (Number 0) (Number 0))) (Number 0)))

parseExpr :: String -> ArithExpr
parseExpr = arithmetize . readTokens

testParse1 = parseExpr "123*-123+123+-123" == Operator Plus (Operator Mult (Number 123) (Number (-123))) (Operator Plus (Number 123) (Number (-123)))
testParse2 = parseExpr "(123*-123)+(123+(-123*321))" == Operator Plus (Operator Mult (Number 123) (Number (-123))) (Operator Plus (Number 123) (Operator Mult (Number (-123)) (Number 321)))

main :: IO ()
main = do
     input <- getLine
     putStrLn (show(eval(parseExpr(input))))