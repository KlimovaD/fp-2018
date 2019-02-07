module Task6 where

import Todo(todo)

data LinkedTree a = Empty | Node a (LinkedTree a) (LinkedTree a) (LinkedTree a)

instance (Show a) => Show (LinkedTree a) where
    show Empty = "[]"
    show (Node v l r _) = 
        "[Node "++ show v ++" "++ show l ++" "++ show r ++"]"

find :: (Ord a) => LinkedTree a -> a -> Bool
find Empty _ = False
find (Node v l r p) a | v > a = find l a
                      | v < a = find r a
                      | v == a = True

newParent Empty a p0 = Node a Empty Empty p0 
newParent (Node v l r p) a p0 | a < v = let n = Node v (newParent l a n) r p0 in n
                              | otherwise = let n = Node v l (newParent r a n) p0 in n

insert ::(Ord a) => LinkedTree a -> a -> LinkedTree a
insert Empty a = Node a Empty Empty Empty
insert (Node v l r p) a | a < v = let n = Node v (newParent l a n) r p in n
                        | otherwise = let n = Node v l (newParent r a n) p in n

joinNode _ Empty Empty = Empty
joinNode p Empty (Node v l r p0) = Node v l r p
joinNode p (Node v l r p0) t = n where n = Node v l (joinNode n r t) p

remove :: (Ord a) => LinkedTree a -> a -> LinkedTree a
remove Empty _ = Empty
remove (Node v l r p) a
    | a < v = Node v (remove l a) r p
    | a > v = Node v l (remove r a) p
    | otherwise = joinNode p l r 


