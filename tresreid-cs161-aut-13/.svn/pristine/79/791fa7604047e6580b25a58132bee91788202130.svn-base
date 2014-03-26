import Data.Char
import Data.List
import System.IO
import Data.Maybe

-- test = "124 * 2 + -312 +300 + 24 * 4" --regular tests work
-- eval (simpleParseExpre) test = Number 332
-- main -- for extracredit 1.
-- -45*2
-- Number 90
-- 25*4 + -360
-- Number (-260)
-- -200*5+1500+-600
-- Number (-100)
--
--
--


data ArithExpre = Number Int| Plus ArithExpre ArithExpre| Mult ArithExpre ArithExpre
    deriving (Show,Eq)

eval :: ArithExpre -> ArithExpre
eval (Number a) = Number a 
eval (Plus (Number a) (Number b)) = Number (a+b)
eval (Mult (Number a) (Number b)) = Number (a*b)

eval (Plus a b) = eval (Plus (eval a) (eval b))
eval (Mult a b) = eval (Mult (eval a) (eval b))

test = "124 * 2 + -312 +300 + 24 * 4"

--simpleParseExpr :: String -> ArithExpre
simpleParseExpre words = buildExpre $ recur redux where

    redux =filter notSpace words
    notSpace :: Char -> Bool
    notSpace c = not (isSpace c)
    
    recur :: String -> [String]
    recur [] = []
    recur (x:xs)
        | isDigit x = (takeWhile (isDigit) (x:xs)) : (recur (dropWhile (isDigit) (x:xs)))
        | x == '-' = ('-':(takeWhile (isDigit) (xs))) : (recur (dropWhile (isDigit) (xs)))
        | otherwise = [x]: (recur xs)
    buildExpre :: [String] -> ArithExpre
    buildExpre list
--       | ("(" `elem` list) == True = buildExpre $ (take (fromJust (elemIndex ")" list)) list) : read(buildExpre $ take (parens 0 0 (tail (snd (break (== "(--") list))) ) (tail ((snd (break (== "(") list))))) : (drop (parens 0 0 (tail (snd (break (== "(") list)))))
        | ("+" `elem` list) == True = Plus (buildExpre  (fst (break (== "+") list ))) (buildExpre (tail (snd (break (=="+") list)))) 
        | ("*" `elem` list) == True = Mult (buildExpre  (fst (break (== "*") list ))) (buildExpre (tail (snd (break (=="*") list)))) 
        | otherwise = Number (read (head list))
    
--    parens :: Int -> Int -> [String]-> Int   
--    parens countp counter (x:xs)
--        | (x == ")") && (countp==0) = counter
--        | (x == "(") = parens (countp+1) (counter+1)(xs)
--        | (x == ")") && (countp/=0) = parens (countp-1) (counter+1)(xs)
--        | otherwise = parens countp (counter+1) (xs)


main :: IO()
main=do
    eq <- getLine
    print $ eval (simpleParseExpre eq)