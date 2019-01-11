module Task2_1 where

import Todo(todo)
import Prelude hiding (lookup)


data TreeMap v = Empty
               | Both Integer v (TreeMap v) (TreeMap v)
               deriving(Show)

emptyTree :: TreeMap v
emptyTree = Empty

-- ���������� �� �������� ���� � ������?
contains :: TreeMap v -> Integer -> Bool

contains' Empty _ = False
contains' (Both key _ l r) k | k == key = True
                             | k > key = contains' r k
                             | k < key = contains' l k
contains t k = contains' t k 

-- �������� ��� ��������� �����
lookup :: Integer -> TreeMap v -> v

lookup' Empty _ = error "not found"
lookup' (Both key value l r) k | k == key = value
                               | k > key = lookup' r k
                               | k < key = lookup' l k

lookup k t = lookup' t k

-- ������� ���� (����, ��������) � ������
insert :: (Integer, v) -> TreeMap v -> TreeMap v

insert' Empty (k, v) = Both k v Empty Empty
insert' (Both key value l r) (k, v) | k == key = Both k v l r
                                    | k > key = Both key value l (insert' r (k, v))
                                    | k < key = Both key value (insert' l (k, v)) r

insert (k, v) t = insert' t (k, v)

-- �������� �������� �� �����
remove :: Integer -> TreeMap v -> TreeMap v

insertIntoLeft Empty lt = lt
insertIntoLeft (Both k v Empty r) lt = Both k v lt r
insertIntoLeft (Both k v l r) lt = Both k v (insertIntoLeft l lt) r

remove' Empty _ = Empty
remove' (Both k v l r) i | i == k = insertIntoLeft r l
                         | i > k = Both k v l (remove' r i)
                         | i < k = Both k v (remove' l i) r

remove i t = remove' t i

nearestLE' :: TreeMap a -> Integer -> (Integer, a)
nearestLE' Empty _ = error "not found"
nearestLE' (Both key value l r) i 
    | i == key = (key, value)
    | i < key = nearestLE' l i
    | i > key = case r of (Both k v _ _) | (i == k) -> (k, v)
                          (Both k v _ _) | (i /= k) -> nearestLE' r i
                          otherwise -> (key, value)

-- ����� ���������� ����� ����� ������������ ���������
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i t = nearestLE' t i

-- ���������� ������ �� ������ ���
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert Empty lst

--listFromTree' :: TreeMap v -> [(Integer, v)]
listFromTree' Empty = []
listFromTree' (Both k v Empty Empty) = [(k, v)]
listFromTree' (Both k v l r) = (listFromTree' l) ++ [(k, v)] ++ (listFromTree' r)

-- ���������� ������ ��� �� ������
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree t = listFromTree' t

kMean'' _ Empty n = error "not found"
kMean'' _ (Both k v Empty Empty) n = n
kMean'' m (Both k v Empty r) n 
    | m == k = n
    | otherwise = kMean'' m r (n + 1)
kMean'' m (Both k v l Empty) n = kMean'' m l (n + 1)
kMean'' m (Both k v l r) n 
    | m == k = kMean'' m l (n + 1)
    | otherwise = (kMean'' m l 1) + (kMean'' m r 1) + n

kMean' Empty _ _ = error "not found"
kMean' t @(Both k v l r) i mn
    | i == m = (k, v)
    | i > m = case r of Empty -> error "not found"
                        otherwise -> kMean' r i (m + 1)
    | i < m = kMean' l i mn
    where m = kMean'' k t mn

-- ����� k-��� ���������� ���������� ������ 
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = kMean' t i 0