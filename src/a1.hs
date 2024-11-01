{- Module A1 of exercise 2.
 - author: Luis Henrique Paoletti
 - date: 25.10.24
 -}
module A1 where
import Types


{- Convert an array of elements into a list. -}
c1 :: [Element] -> List
{- Convert a list into an array of its elements. -}
c2 :: List -> [Element]
{- Concatenate a list onto another. -}
c :: List -> List -> List
{- Concatenate an element onto a list. -}
ce :: List -> Element -> List
{- Access the element in the list at the given index. -}
a :: List -> Index -> Element
{- Get the size of a list. -}
listSize  :: List -> Int
listSize' :: AccInt -> List -> Int


c1 []     = E
c1 (e:es) = L e (c1 es)

c2 E       = []
c2 (L e l) = e:(c2 l)

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

listSize = listSize' 0
listSize' acc E       = acc
listSize' acc (L e l) = listSize' (acc + 1) l
