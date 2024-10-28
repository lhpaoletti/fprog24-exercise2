{- Main module of exercise 2.
 - author: Luis Henrique Paoletti
 - date: 25.10.24
 -}
module Exercise2 where
import Types
import A1
import A2

{- Whether a list and a list' are equal by these conditions:
 - 1. They have the same length
 - 2. They are equal element-wise
 -}
eq :: List -> List' -> Bool
eq E E' = True
eq E _  = False
eq _ E' = False  -- this line makes it safe to use fst and shift bellow
eq (L e l) l' = e == fst l'
                && eq l (shift l')
  where fst (L' E' x) = x     -- get first element of a non-empty List'
        fst (L' xs _) = fst xs
        shift (L' E' _) = E'  -- shift a non-empty List' to the left, "cutting" the first element
        shift (L' xs x) = L' (shift xs) x
