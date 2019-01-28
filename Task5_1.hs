module Task5_1 where

import Todo(todo)

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
         let rec = DCons left h (list2dlist' rec t)
         in rec

index :: DList a -> Int -> a
index (DCons l c r) i | i < 0 = error "negative index"
             | otherwise = if i == 0 then c else index r (i-1)
                         
    


insertAt :: DList a -> Int -> a -> DList a
insertAt _ i _  | i < 0 = error "negative index"
insertAt DNil _ el  = DCons DNil el DNil
insertAt (DCons l c r) 0 el = insert' where
        insert' = DCons l el (DCons insert' c r)
insertAt (DCons l c DNil) i el = insert' where
        insert' = DCons l c (DCons insert' el DNil)
insertAt (DCons l c r) i el = DCons l c (insertAt r (i-1) el)



removeAt :: DList a -> Int -> DList a
removeAt _ i | i < 0 = error "negative index"
removeAt DNil _ = DNil
removeAt (DCons DNil c (DCons l c' r)) 0 = DCons DNil c' r
removeAt (DCons _ c DNil) i | i == 0 = DNil
removeAt (DCons (DCons ll lc lr) c (DCons rl rc rr)) 0 = remove' where
        remove' = DCons (DCons ll lc remove') rc rr
removeAt (DCons l c r) i = DCons l c (removeAt r (i - 1))