
module Imp_State (State, emp, valof, update)  where

emp :: State
valof :: State -> String -> Integer
update :: State -> String -> Integer -> State

-- states represented as lists of (location, value) pairs
-- but the representation is hidden under the State constructor
-- which is not exported from the module (only the type State is exported)

newtype State = State [(String,Integer)] deriving Show

emp = State [] -- new empty state

valof (State lns) l =   -- values of locations in state
  case (lookup l lns) of
     (Just n) -> n
     _ -> error ("Lookup error - unassigned location: " ++ l)

update (State lns) l n = -- update state
  State ((l,n) : [(l',n') | (l',n') <- lns, l' /= l])


