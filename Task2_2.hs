module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x [] = x
foldl f x (h : t) = foldl f (f x h) t

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x [] = x
foldr f x (h : t) = f h (foldr f x t)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
                Just (a, b') -> a : unfoldr f b'
                Nothing -> []

-- ����� ���� ��������� ������ (������)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- ��������� ������ 
reverse :: [a] -> [a]
reverse [] = []
reverse list = foldl (\t h -> h:t) [] list

-- ����������� ��������� ������
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f list = foldr (\h t -> (f h):t) [] list

-- ������������ ���� ��������� ������
product :: [Integer] -> Integer
product lst = foldl (*) 1 lst

-- ��������� �� ������ Maybe ���� ������������ ��������
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (maybe id (:)) []

-- ��������� �������
diagonal :: [[a]] -> [a]
diagonal = reverse . fst . foldl(\(r,c) s -> ((s !! c):r,c+1)) ([],0)

-- ������ ��� ���� ���������, �� ��������������� ���������
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot _ [] = []
filterNot f list = foldr (\h t -> if (f h) then (h:t) else t) [] list

-- ����� �������� � ������
elem :: (Eq a) => a -> [a] -> Bool
elem w = foldr (\x -> (||) (w == x)) False

-- ������ ����� � ��������� [from, to) � ����� step
rangeTo :: Integer -> Integer -> Integer -> Integer
rangeTo from to step = foldl (\ acc x -> if (x `mod` step == z) then acc ++ [x] else acc) [] [from..to-1] where z=from `mod` step

-- ������������ ���� �������
append :: [a] -> [a] -> [a]
append [] [] = []
append list1 list2 = foldr (\h t -> h:t) list2 list1

-- ��������� ������ lst �� ����� �������� n
-- (��������� ����� ����� ���� ������)
groups :: [a] -> Integer -> [[a]]
groups lst n = let (l,y,_)= foldl (\(r,a,i) x -> if i == n then (reverse a:r,[x],1) else (r,x:a,i+1)) ([],[],0) lst in reverse $ reverse y : l