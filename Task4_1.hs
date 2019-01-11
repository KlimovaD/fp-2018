module Task4_1 where

-- ������ ��� ��������. � �������� �������� �������� `fun` ����� ���� ��� ������
-- ����������, ������ �� �� `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- ���������� ������ `Functor`, `Applicative` � `Monad` ��� ���� `FunMonad`

 
instance Functor FunMonad where
 fmap f (FunMonad g) = FunMonad (f . g)

instance Applicative (FunMonad) where
    pure = return
    f <*> x = f >>= \f' -> x >>= \x' -> return (f' x')

instance Monad FunMonad where
    return x = FunMonad(\s -> x)
    (FunMonad fx) >>= f = FunMonad (\s -> (fun $ f $ (fx s)) $ s)