{- Type module for exercise 2.
 - author: Luis Henrique Paoletti
 - date: 28.10.24
 -}
module Types where

type Element = Char
type Index   = Int
data List    = E                 -- E for "empty"
               | L Element List  -- L for "link"
               deriving (Eq, Show)

type AccInt    = Int  -- int accumulator
type BackIndex = Int  -- index starting at 0 but from behind
data List'     = E'                  -- E for "empty"
                 | L' List' Element  -- L for "link"
                 deriving (Eq, Show)
