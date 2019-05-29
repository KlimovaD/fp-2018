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

newParent Empty _ = Empty 
newParent (Node v l r p) n = Node v l r n

insert ::(Ord a) => LinkedTree a -> a -> LinkedTree a
insert Empty a = Node a Empty Empty Empty
insert (Node v l r p) a | a < v = n 
                        | otherwise = n1 
                        where n = Node v (insert l a) rt (insert p a) 
                              n1 = Node v lt (insert r a) (insert p a) 
                              lt = newParent l n1
                              rt = newParent r n


joinNode _ Empty Empty = Empty
joinNode Empty (Node v l r p0) p = Node v l r p
joinNode (Node v l r p0) t p = n where n = Node v l (joinNode r t n) p

remove :: (Ord a) => LinkedTree a -> a -> LinkedTree a
remove Empty _ = Empty
remove (Node v l r p) a
    | a < v = n
    | a > v = n1
    | otherwise = joinNode l r p
    where n = Node v (remove l a) rt (remove p a)
          n1 = Node v lt (remove r a) (remove p a)
          rt = newParent r n
          lt = newParent l n1