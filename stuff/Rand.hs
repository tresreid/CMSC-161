module Rand where
import UCState
import System.Random
import System.IO.Unsafe
--import Control.Monad.State.Lazy hiding (put, get, State)

type RandState a = State StdGen a

runRandom :: RandState a -> StdGen -> a
runRandom (State f) s = res
    where (res, state) = f s

randR :: Random a => (a,a) -> RandState a
randR bound = do
    gen <- get
    let (x, gen') = randomR bound gen
    put gen'
    return x

rollTwoDice ::RandState Int
rollTwoDice = do
    d1 <- randR (1,6)
    d2 <- randR (1,6)
    let d = d1 + d2
    return d

    -- Data types to represent playing cards
data CardValue = King | Queen | Jack | NumberCard Int
    deriving (Show, Eq)
data CardSuit = Hearts | Diamonds | Spades | Clubs
    deriving (Show, Eq)
data PlayingCard = PlayingCard CardSuit CardValue
    deriving (Show, Eq)

{-
 - fullCardDeck will be a deck of cards, 52 in total, with a King, a Queen, 
 - a Jack and NumberCards from 1 to 10 for each suit.
 -}
fullCardDeck :: [PlayingCard]
fullCardDeck = [ PlayingCard s v | s <- allsuits, v <- allvals ] where
        allvals = King : Queen : Jack : [ NumberCard i | i <- [1..10] ]
        allsuits = [Hearts, Diamonds, Spades, Clubs]

removeCard :: [PlayingCard] -> RandState [PlayingCard]
removeCard cards =do 
    cardNum <- randR (1, (length cards))
    let card = cards !! (cardNum-1)
    let newCards = (take (cardNum -1) cards) ++ (drop cardNum cards)
    return $ card: newCards

shuffleDeck :: [PlayingCard]-> RandState [PlayingCard]
shuffleDeck [] = return []
shuffleDeck cards = do

   (y:ys) <- removeCard cards
   zs <- shuffleDeck ys
   return $ y:zs

testShuffleDeck :: IO [PlayingCard]
testShuffleDeck = do
    gen <- newStdGen
    return $ runRandom (shuffleDeck fullCardDeck) gen

{-type Point = (Double,Double)

rand :: Random a => RandState a
rand = do
    gen <- get
    let (x, gen') = random gen
    put gen'
    return x

makePointandCheck :: RandState Bool 
makePointandCheck= do
    x <- randR (-1,1)
    y <- randR (-1,1)
    let x' = x^2
    let y' = y^2
    let z = sqrt( x'+y')
    let z' = checkCircle z
    return z'

checkCircle z
    | ((z < 1)== True) = True
    | otherwise = False


addCount counter in' out 
    | counter >= 100 = ((4*in')/ out)
    |(makePointandCheck)== tBool = addCount (counter+1) (in'+1) out
    |(makePointandCheck)== fBool = addCount (counter +1) in' (out +1)
    | otherwise = ((4*in') /out)
    where tBool =do
              x <- randR (1,1)
              y <- randR (1,1)
              return (x==y)
          fBool = do
              x <- randR(1,1)
              y <- randR (2,2)
              return (x==y)
-}