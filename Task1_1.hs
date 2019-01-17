module Task1_1 where

import Todo(todo)

data Operation = Plus | Minus | Multiply
            deriving(Show,Eq)

data Term = IntConstant{ intValue :: Int }           
            | Variable{ varName :: String }          
            | BinaryTerm{ lhv :: Term, rhv :: Term, oper :: Operation } 
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
replaceVar varName replacement expression = case (expression) of
          Variable var | var == varName -> replacement
          BinaryTerm lhv rhv oper -> BinaryTerm (replaceVar varName replacement lhv) (replaceVar varName replacement rhv) oper
          _ -> expression


evaluate :: Term -> Term
evaluate (BinaryTerm l r oper) = 
    let lhv = evaluate l in
    let rhv = evaluate r in
    case oper of
    Plus -> case (lhv, rhv) of
        (IntConstant lhv, IntConstant rhv) -> IntConstant (lhv + rhv)
        (IntConstant 0, rhv) -> rhv
        (lhv, IntConstant 0) -> lhv
        otherwise -> BinaryTerm lhv rhv Plus
    Minus -> case (lhv, rhv) of
        (IntConstant lhv, IntConstant rhv) -> IntConstant (lhv - rhv)
        (lhv, IntConstant 0) -> lhv
        otherwise -> BinaryTerm lhv rhv Minus
    Multiply -> case (lhv, rhv) of
        (IntConstant lhv, IntConstant rhv) -> IntConstant (lhv * rhv)
        (IntConstant 0, rhv) -> IntConstant 0
        (IntConstant 1, rhv) -> rhv
        (lhv, IntConstant 0) -> IntConstant 0
        (lhv, IntConstant 1) -> lhv
        otherwise -> BinaryTerm lhv rhv Multiply
    otherwise -> BinaryTerm l r oper
evaluate expression = expression