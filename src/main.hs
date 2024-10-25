{- Main module of exercise 2.
 - author: Luis Henrique Paoletti
 - date: 25.10.24
 -}

type Element = Char
type Index   = Int
data List    = E                 -- E for "empty"
               | L Element List  -- L for "link"
               deriving (Eq, Show)


{- Convert an array of elements into a list. -}
c1 :: [Element] -> List
{- Convert a list into an array of its elements. -}
c2 :: List -> [Element]

c1 []     = E
c1 (e:es) = L e (c1 es)

c2 E       = []
c2 (L e l) = e:(c2 l)
