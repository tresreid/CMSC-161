import Data.Char
import Data.List
import System.IO
import Data.Maybe

--Tests
-- >let expr = Plus (Number 1) (Plus (Variable "x") (Variable "y"))
-- >let vars = [("x",2),("y",-4),("z",2)]
-- >eval expr vars
--Number (-1)

-- > simplify (d "x" (Mult (Mult (Variable "x") (Variable "x")) (Variable "x")))
-- Plus (Mult (Plus (Varible "x") (Variable "x")) (Variable "x")) (Mult (variable "x") (Variable "x"))

-- > eval (simpleParseExpre test assListTest
-- Number 30
data ArithExpre = Number Integer| Plus ArithExpre ArithExpre| Mult ArithExpre ArithExpre| Variable String
    deriving (Show,Eq)

eval :: ArithExpre -> [(String,Integer)]->ArithExpre
eval (Number a) assList= Number a 
eval (Plus (Number a) (Number b)) assList = Number (a+b)
eval (Mult (Number a) (Number b)) assList= Number (a*b)
eval (Variable v) assList = eval (Number (getVars)) assList where
    getVars = snd $ head $ filter ((==v).fst) assList
    

eval (Plus a b) assList= eval (Plus (eval a assList) (eval b assList)) assList
eval (Mult a b) assList= eval (Mult (eval a assList) (eval b assList)) assList

test = "2 * t + 2* xkcd "
assListTest = [("%t",5),("%xkcd",10)]
--simpleParseExpr :: String -> ArithExpre
simpleParseExpre words =  buildExpre $ recur redux where

    redux =filter notSpace words
    notSpace :: Char -> Bool
    notSpace c = not (isSpace c)
    
    recur :: String -> [String]
    recur [] = []
    recur (x:xs)
        | isLetter x = ('%':(takeWhile (isLetter) (x:xs))) : (recur (dropWhile (isLetter) (x:xs)))
        | x == '-' = ('%':'-':(takeWhile (isLetter) (xs))) : (recur (dropWhile (isLetter) (xs)))
        | isDigit x = (takeWhile (isDigit) (x:xs)) : (recur (dropWhile (isDigit) (x:xs)))
        | x == '-' = ('-':(takeWhile (isDigit) (xs))) : (recur (dropWhile (isDigit) (xs)))
        | otherwise = [x]: (recur xs)
    buildExpre :: [String] -> ArithExpre
    buildExpre list
        | ("+" `elem` list) == True = Plus (buildExpre  (fst (break (== "+") list ))) (buildExpre (tail (snd (break (=="+") list)))) 
        | ("*" `elem` list) == True = Mult (buildExpre  (fst (break (== "*") list ))) (buildExpre (tail (snd (break (=="*") list)))) 
        | ('%' `elem` (head list)) == True = Variable (head list)
        |otherwise = Number (read (head list))
    
d :: String -> ArithExpre -> ArithExpre
d s (Plus f g) = Plus (d s f) (d s g)
d s (Mult f g) = Plus (Mult (d s f) g) (Mult (d s g) f)
d s (Number a) = Number 0
d s (Variable x)
    | x /= s = Number 0
    | otherwise = Number 1

simplify :: ArithExpre -> ArithExpre
simplify (Number a) = Number a
simplify (Variable x) = Variable x
simplify (Plus (Number 0) x) = x
simplify (Plus x (Number 0)) = x
simplify (Mult x (Number 1)) = x
simplify (Mult (Number 1) x) = x
simplify (Mult (Number 0) x) = Number 0
simplify (Mult x (Number 0) = Number 0
simplify (Plus a b) = Plus (simplify a) (simplify b)
simplify (Mult a b) = Mult (simplify a) (simplify b)


--main :: IO()
--main=do
--    eq <- getLine
--    print $ eval (simpleParseExpre eq) 