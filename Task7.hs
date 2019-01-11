module Task7 where

data Deque a = Deque [a] [a]

empty :: Deque a
empty = Deque [] []

pushFront :: Deque a -> a -> Deque a
pushFront (Deque inp out) x = Deque (x:inp) out

pushBack :: Deque a -> a -> Deque a
pushBack (Deque inp out) x = Deque inp (x:out)

takeHalf :: [a] -> [a]
takeHalf list  = take (fromInteger $ floor $ (toRational $ length list)/2) list

popBack :: Deque a -> (a, Deque a)
popBack (Deque [] []) = error "Empty deque"
popBack (Deque inp (oe:ot)) = (Deque inp ot, oe)
popBack (Deque inp []) = popBack $ Deque (takeHalf inp) (takeHalf $ reverse inp)
                      

popFront :: Deque a -> (a, Deque a)
popFront (Deque [] []) = error "Empty queue"
popFront (Deque (ih:it) out) = (Deque it out, ih)
popFront (Deque [] out) = popFront $ Deque (takeHalf $ reverse out) (takeHalf out)

{-
����� �������. ����� �������� ���� ������� N ����� ������� �� ����� N$. ��� ��� ��� ������ ���������� ������ �������� ������ "���������������" � ������ �������������, ����� ���� ����������, ��� �������� ������� �� ����������� ������ ����� ����������� �� ���� ���� � N/2, �� ���� ��������� ����� ��������. ����� ������� ���������������� �������� - ���������.
����� ������ � ������ ������ ����� �� ���������� �� �������. �� ��������� ������� ������ ������� - ����� ��������� ���� ������. ���������� ���� �� � ���� �� ������ ����������� ��������� �� 1, �������� ��� ����������� �� �������� ���������, � ������������� ����� ��������� ��������� �� N � �������� O(N). ��������� �� ����� ��������� �������������, ��� ��� �������������� ���������� ��������� �� ����� ���� ������ ������� �������, � ����� ��������������� �� ����� ���� ����� N/2 ��������.
-}