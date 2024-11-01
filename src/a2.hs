{- Module A2 of exercise 2.
 - author: Luis Henrique Paoletti
 - date: 25.10.24
 -}
module A2 where
import Types


{- Convert an array of elements into a list. -}
c1'  :: [Element] -> List'
c1'' :: [Element] -> List'
{- Convert a list into an array of its elements. -}
c2'  :: List' -> [Element]
c2'' :: List' -> [Element]
{- Concatenate a list onto another. -}
c' :: List' -> List' -> List'
{- Push an element into the front of a list. -}
push :: List' -> Element -> List'
{- Access the element in the list at the given index. -}
a'  :: List' -> Index -> Element
a'' :: List' -> BackIndex -> Element
{- Get the size of a list. -}
list'Size  :: List' -> Int
list'Size' :: AccInt -> List' -> Int


c1' = c1'' . reverse
c1'' [] = E'
c1'' (e:es) = L' (c1'' es) e

c2' = reverse . c2''
c2'' E'       = []
c2'' (L' l e) = e:(c2'' l)

c' E' E' = E'
c' l  E' = l
c' E' l  = l
c' (L' l1 e1) (L' l2 e2) = c' l1 (L' (push l2 e1) e2)

push E' e         = L' E' e
push (L' l e1) e2 = L' (push l e2) e1

a' l i = let length  = list'Size l
             backInd = length - i - 1
         in if i >= length
            then error "Index out of bounds"
            else a'' l backInd
a'' (L' l e) 0 = e
a'' (L' l _) i = a'' l (i - 1)

list'Size = list'Size' 0
list'Size' acc E'       = acc
list'Size' acc (L' l _) = list'Size' (acc + 1) l
