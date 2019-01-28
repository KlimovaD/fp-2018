module Task2_1 where

import Todo(todo)
import Prelude hiding (lookup)

data TreeMap v = Empty
               | Both Integer v (TreeMap v) (TreeMap v)
               deriving(Show)

emptyTree :: TreeMap v
emptyTree = Empty

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains Empty _ = False
contains (Both key _ l r) k | k > key = contains r k
                            | k < key = contains l k
                            | otherwise = true

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup Empty _ = error "empty tree"
lookup k (Both key value l r) | k == key = value
                              | k > key = lookup k r
                              | k < key = lookup k l

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) Empty = Both k v Empty Empty
insert (k, v) (Both key value l r)
    | k < key   = Both key value (insert (k, v) l) r
    | k > key   = Both key value l (insert (k, v) r)
    | otherwise = (Both key value l r)

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove _ Empty = error "empty tree"
remove i (Both key value l r)
    | i < key  = Both key value (remove i l) r
    | i > key  = Both key value l (remove i r)
    | otherwise = case (l, r) of
                      (Empty, Empty) -> Empty
                      (l, Empty) -> l
                      (Empty, r) -> r
                      (l, r) -> remove' l r
   where remove' l' Empty = l'
         remove' l' (Both key value Empty r) = Both key value l' r
         remove' l' (Both key value l r) = Both key value (remove' l' l) r

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE _ Empty = error "empty tree"
nearestLE k (Both key value l r)
    | k < key  = nearestLE i l
    | k > key  = case r of 
        Both key' value' l' _ | k == key' -> (key', value')
                              | k > key'  -> nearestLE k r
                              | k < key'  -> case (l') of 
                                        Empty -> (key, value)
                                        _ -> nearestLE k l'
    | otherwise -> (key, value) 

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert Empty lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree tree = case tree of 
   Empty -> []
   Both key value l r -> concat [listFromTree l, [(key, value)], listFromTree r]

-- Поиск k-той порядковой статистики дерева 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ Empty = error "empty tree"
kMean k (Both key value l r)
    | k == (size l) = (key, value)
    | k < (size l)  = kMean k l
    | k > (size l)  = kMean (k - size l - 1) r
    where size tree = case tree of 
                      Empty -> 0
                      (Both _ _ l r) -> size l + 1 + size r