module Task5_2 where

import Todo(todo)
import Data.Ratio

data Stream a = Cons {
                    shead :: a,
                    stail :: Stream a
                }

srepeat :: a -> Stream a
srepeat x =
    let rec = Cons x rec in
    rec

generate :: a -> (a -> a) -> Stream a
generate x f =
    Cons x $ generate (f x) f

instance Functor Stream where
    fmap f (Cons h t) = Cons (f h) (fmap f t)

diag (Cons h t) = Cons (shead h) $ diag (stail <$> t)
sflatten = diag

instance Applicative Stream where
    pure x = srepeat x
    f <*> x = do { f' <- f ; x' <- x ; return $ f' x' }

instance Monad Stream where
    return x = srepeat x
    ss >>= f = sflatten (f <$> ss)

sin' :: Double -> Integer -> Double
sin' x n = sum $ fmap (monom x) [1..n] 
  where monom x n = (if even (n - 1) then 1 else (-1)) * product (fmap ((x/) . fromInteger) [1..(2*n - 1)])


sinPrecisions :: Double -> Stream Double
sinPrecisions x = sinPrecisions' x 1 where
    sinPrecisions' x n = sin' x n <:> sinPrecisions' x (n+1)


ePrecisions :: Stream Rational
ePrecisions = todo