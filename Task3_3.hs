module Task3_3 where

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- сложение множеств
instance Monoid (PSet a) where
  mempty = PSet (\a -> False)
  mappend (PSet f1) (PSet f2) = PSet (\a -> (f1 a) || (f2 a))

-- пересечение множеств
instance Monoid (PSet2 a) where
  mempty = PSet (\a -> True)
  mappend (PSet f1) (PSet f2) = PSet (\a -> (f1 a) && (f2 a))

-- симметрическая разность
instance Monoid (PSet a) where
  mempty = PSet (\a -> False)
  mappend (PSet f1) (PSet f2) = PSet (\a -> ((f1 a) && (not $ f2 a)) || ((not $ f1 a) && (f2 a)))

-- функтор
-- Результат всегда возвращает False, потому что зная только отображение из множества A в B 
-- мы не можем сказать ничего о множестве B 
instance Functor PSet where
  fmap f (PSet fa) = PSet (\b -> False)