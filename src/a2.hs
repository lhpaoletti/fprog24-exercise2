{- Module A2 of exercise 2.
 - author: Luis Henrique Paoletti
 - date: 25.10.24
 -}
module A2 where

type Element = Char
type Index   = Int
data List    = E                 -- E for "empty"
               | L List Element  -- L for "link"
               deriving (Eq, Show)


{- Convert an array of elements into a list. -}
c1  :: [Element] -> List
c1' :: [Element] -> List
{- Convert a list into an array of its elements. -}
c2  :: List -> [Element]
c2' :: List -> [Element]
{- Concatenate a list onto another. -}
{-
c :: List -> List -> List
{- Concatenate an element onto a list. -}
ce :: List -> Element -> List
{- Access the element in the list at the given index. -}
a :: List -> Index -> Element
-}


c1 = c1' . reverse
c1' [] = E
c1' (e:es) = L (c1' es) e

c2 = reverse . c2'
c2' E       = []
c2' (L l e) = e:(c2' l)

{-
c E E = E
c l E = l
c E l = l
c l (L e l2) = c (ce l e) l2

ce E e         = L e E
ce (L e1 l) e2 = L e1 (ce l e2)

a E _ = error "Index out of bounds"
a (L e l) i = if i == 0
              then e
              else a l (i - 1)
-}
