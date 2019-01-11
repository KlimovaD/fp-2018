module Task1_2 where

import Todo(todo)

import Data.Maybe

-- является ли дата корректной с учётом количества дней в месяце и вискокосных годов?
isDateCorrect :: Int -> Int -> Int -> Bool
isDateCorrect d m y = d>0 && m>0 && m<13 && y>=0 && d<=
    (if m `elem` [4,6,9,11] then 30
     else 
        if m == 2 then
            if (y `mod` 400 == 0) || (y `mod` 100 /= 0) && 
                (y `mod` 4 == 0) then 29
            else 28
        else 31)
 
-- наибольший общий делитель двух чисел
gcd_ :: Integer -> Integer -> Integer
gcd_ a b | a == b = a
        | a > b = if a `mod` b == 0 then b else gcd_ b (a - b)
        | otherwise = gcd_ b a

--Возведение числа в степень
pow :: Integer -> Integer -> Integer
pow x y | y == 0 = 1
        | x == 0 = 0
        | y `mod` 2 == 1 = x * pow x (y - 1)
        | y `mod` 2 == 0 = pow (x * x) (y `div` 2) 

-- является ли данное число простым
isPrime :: Integer -> Bool
isPrime x = factors x == [1,x] where factors x = [n | n <- [1..x], mod x n == 0]

-- рассчитайте площадь многоугольника
data Point2D = Point2D {x :: Double, y :: Double}
shapeArea :: [Point2D] -> Double
shapeArea [] = 0
shapeArea lst = 
    let xs = map x lst
        ~ys@(yh:yt) = map y lst
    in 0.5 * abs (sum [xi*(yp-yn)|(xi,yp,yn) <- zip3 xs (yt ++ [yh]) (last ys : ys)])