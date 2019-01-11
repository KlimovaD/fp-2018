module Task3_3 where

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- �������� ��������
instance Monoid (PSet a) where
  mempty = PSet (\a -> False)
  mappend (PSet f1) (PSet f2) = PSet (\a -> (f1 a) || (f2 a))

-- ����������� ��������
instance Monoid (PSet2 a) where
  mempty = PSet (\a -> True)
  mappend (PSet f1) (PSet f2) = PSet (\a -> (f1 a) && (f2 a))

-- �������������� ��������
instance Monoid (PSet a) where
  mempty = PSet (\a -> False)
  mappend (PSet f1) (PSet f2) = PSet (\a -> ((f1 a) && (not $ f2 a)) || ((not $ f1 a) && (f2 a)))

-- �������
-- ��������� ������ ���������� False, ������ ��� ���� ������ ����������� �� ��������� A � B 
-- �� �� ����� ������� ������ � ��������� B 
instance Functor PSet where
  fmap f (PSet fa) = PSet (\b -> False)