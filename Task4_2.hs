module Task4_2 where

data FourOf a = FourOf a a a a deriving(Show,Eq)

instance Functor FourOf where
    fmap f (FourOf x y z t) = FourOf (f x) (f y) (f z) (f t)
 
instance Applicative FourOf where
    pure x    = FourOf x x x x
    (FourOf fx fy fz ft) <*> (FourOf x y z t) = FourOf (fx x) (fy y) (fz z) (ft t)
 
instance Monad FourOf where
    (FourOf x y z t) >>= f = let (FourOf x' _ _ _) = f x
                                 (FourOf _ y' _ _) = f y
                                 (FourOf _ _ z' _) = f z
                                 (FourOf _ _ _ t') = f t
                             in FourOf x' y' z' t'