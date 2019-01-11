module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList rev = 
    let listed (RNil) = []
        listed (RCons i e) = e : listed i in
    reverse $ listed rev

listToRList :: [a] -> ReverseList a
listToRList list = foldl (\ acc el -> RCons acc el) RNil list

-- util
showAsList :: (Show a) => ReverseList a -> String
showAsList RNil = ""
showAsList (RCons RNil h) = show h
showAsList (RCons tl hl) = (showAsList tl) ++ ", " ++ (show hl)

instance (Eq a) => Eq (ReverseList a) where
  (==) RNil RNil = True
  (==) RNil _ = False
  (==) _ RNil = False
  (==) (RCons tl hl) (RCons tr hr) = hl == hr && tl == tr

instance (Ord a) => Ord (ReverseList a) where
  (<=) RNil RNil = True
  (<=) RNil _ = True
  (<=) _ RNil = False
  (<=) (RCons tl hl) (RCons tr hr) = if (hl <= hr) then True
                                      else tl <= tr

instance (Show a) => Show (ReverseList a) where
  show RNil = "[]"
  show list = "[" ++ (showAsList list) ++ "]"

instance Monoid (ReverseList a) where
  mempty = RNil
  mappend RNil list = list
  mappend list RNil = list
  mappend list (RCons tl hl) = RCons (mappend list tl) hl

instance Functor ReverseList where
  fmap f RNil = RNil
  fmap f (RCons tl hl) = RCons (fmap f tl) (f hl)