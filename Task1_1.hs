module Task1_1 where

import Todo(todo)

data Term = IntConstant{ intValue :: Int }          
            | Variable{ varName :: String }         
            | BinaryTerm{ lhv :: Term, rhv :: Term } 
            deriving(Show,Eq)

infixl 6 |+|
(|+|) :: Term -> Term -> Term
(|+|) (IntConstant l) (IntConstant r) = IntConstant (l + r)
(|+|) l r = BinaryTerm l r 

infixl 6 |-|
(|-|) :: Term -> Term -> Term
(|-|) (IntConstant l) (IntConstant r) = IntConstant (l - r)
(|-|) l r = BinaryTerm l r 

infixl 7 |*|
(|*|) :: Term -> Term -> Term
(|*|) (IntConstant l) (IntConstant r) = IntConstant (l * r)
(|*|) l r = BinaryTerm l r 

replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (IntConstant int) = IntConstant int
replaceVar varName replacement (Variable var) = if (var == varName) 
                                                then replacement 
                                                else Variable var
replaceVar varName replacement (BinaryTerm l r) = BinaryTerm 
                                                    (replaceVar varName replacement l) 
                                                    (replaceVar varName replacement r)

evaluate :: Term -> Term
evaluate (Variable var) = Variable var
evaluate (IntConstant int) = IntConstant int
evaluate (BinaryTerm (IntConstant 0) (IntConstant r)) = IntConstant (r) 
evaluate (BinaryTerm (IntConstant l) (IntConstant 0)) = IntConstant (l) 
evaluate (BinaryTerm (IntConstant l) (IntConstant r)) = IntConstant (l + r) 
evaluate (BinaryTerm l r) = BinaryTerm l r