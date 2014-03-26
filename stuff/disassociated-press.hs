module Main where

import Control.Monad.State
import Data.Map (Map,(!))
import Data.Maybe
import qualified Data.Map as Map
import System.Random
import System.IO

type RandState = State StdGen
type Model = (String,Map String [Maybe String])

mkModel :: [String] -> Model
mkModel xs@(x:_) = (x,mkMap xs) where
    mkMap = foldr combine Map.empty . mkPairs
    combine (k,v) = Map.alter (\vs -> fmap (v:) vs `mplus` return [v]) k
    mkPairs (x:ys@(y:_)) = (x,Just y) : mkPairs ys
    mkPairs [x] = [(x,Nothing)]

runModel :: Model -> RandState [String]
runModel (start,wordmap) = iter start where
    iter word = do
        succ <- select $ wordmap ! word
        case succ of
            Nothing -> return [word]
            Just w -> fmap (word:) $ iter w

select :: [a] -> RandState a
select as = fmap (as !!) $ state $ randomR (0,length as - 1)

linefill :: Int -> [String] -> String
linefill _ [] = "\n"
linefill n (x:xs) = iter x xs where
    iter x (y:ys)
        | length x + length y + 1 > n = x ++ "\n" ++ linefill n (y:ys)
        | otherwise                   = iter (x ++ " " ++ y) ys
    iter x [] = x ++ "\n"

findLargest:: [String] ->String
findLargest list = snd $ maximum $ map (\x -> (length x, x)) list

       

main :: IO ()
main = do
    input <- getContents
    let model = mkModel (words input)
    gen <- getStdGen
    let ws = evalState (runModel model) gen
    let big = findLargest ws
    putStr $ big


