import Data.Maybe

{- TESTS
>let h= (Node 5 (Node 4 Empty Empty) (Node 3 (Node 2 Empty Empty) (Node 1 Empty Empty)))
>let h1 = insert h 4
>h1
Node 5 (Node 4 (Node 3 (Node 2 Empty Empty) Empty) (Node 1 Empty Empty)) (Node 4 Empty Empty)
> let h2 = delete h1
>h2
Node 4 (Node 3 (Node 2 Empty Empty) (Node 1 Empty Empty)) (Node 4 Empty Empty)
> let h3 = updateRoot h2 6
>h3
Node 6 (Node 3 (Node 2 Empty Empty) (Node 1 Empty Empty)) (Node 4 Empty Empty)
>let h4 = updateRoot h3 2
>h4
Node 4 (Node 2 Empty Empty) (Node 3 (Node 2 Empty Empty) (Node 1 Empty Empty))
> let hs = heapsort [32,34,65,7,3,765,2,86,4653,8,536,22,22,53,15234]
>hs
[2,3,7,8,22,22,32,34,53,65,86,536,765,4653,15234]
>let hs' = heapsort ['q','w','e','r','t','y','u','i','o','p','a','s','d','f','g','h','j','k','l','z','x','c','v','b','n','m']
>hs' = "abcdefghijklmnopqrstuvwxyz"
-}
data Heap a = Empty | Node a (Heap a) (Heap a)
    deriving (Show)

insert :: (Ord a) => Heap a -> a -> Heap a
insert Empty x = (Node x Empty Empty)
insert (Node a b c) x
    | x <=a = (Node a (insert c x) b)
    | x > a = (Node x (insert b a) c)

getMax :: (Ord a) => Heap a -> Maybe a
getMax Empty = Nothing
getMax (Node x a b) = Just x

delete :: (Ord a) => Heap a -> Heap a
delete (Node _ Empty b) = b
delete (Node _ a Empty) = a
delete (Node _ a b)
    | (getMax a) >= (getMax b) = (Node (fromJust (getMax a)) (delete a) b)
    | (getMax a) <  (getMax b) = (Node (fromJust (getMax b)) (delete b) a)

updateRoot :: (Ord a) => Heap a -> a -> Heap a
updateRoot Empty x = (Node x Empty Empty)
updateRoot (Node _ b c) x
    | (((Just x) >= getMax b) && ((Just x) >= getMax c)) = (Node x b c)
    | ((getMax b >= getMax c) && (getMax b >= (Just x))) = (Node (fromJust (getMax b)) (updateRoot b x) c)
    | ((getMax b <  getMax c) && (getMax b >= (Just x))) = (Node (fromJust (getMax c)) (updateRoot c x) b) 

listToHeap :: (Ord a) => [a] -> Heap a -> Heap a
listToHeap [] heap = heap
listToHeap (x:xs) heap = listToHeap xs (insert heap x)

heapToList :: (Ord a) => Heap a -> [a] -> [a]
heapToList Empty list = list
heapToList heap list = heapToList (delete heap) ((fromJust (getMax heap)):list)

heapsort :: (Ord a) => [a] -> [a]
heapsort list =  heapToList (listToHeap list Empty) []

