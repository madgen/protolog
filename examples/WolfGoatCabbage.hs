{-# LANGUAGE OverloadedStrings #-}
module WolfGoatCabbage where

import qualified Language.Protolog.StdLib.List as List
import Language.Protolog

wolf, goat, cabbage, farmer, vacant :: Term
wolf = Fx "wolf" []
goat = Fx "goat" []
cabbage = Fx "cabbage" []
farmer = Fx "farmer" []
vacant = Fx "vacant" []

bank :: Term -> Term -> Term -> Term -> Term
bank f w g c = Fx "bank" [f, w, g, c]

state :: Term -> Term -> Term
state h m = Fx "state" [h, m]

move :: Term -> Term -> Atom
move stBefore stAfter = Atom "move" [ stBefore, stAfter ]

bimove :: Term -> Term -> Atom
bimove stBefore stAfter = Atom "bimove" [ stBefore, stAfter ]

start, end :: Term -> Atom
start t = Atom "start" [ t ]
end t = Atom "end" [ t ]

safe, unsafe :: Term -> Atom
safe t = Atom "safe" [ t ]
unsafe t = Atom "unsafe" [ t ]

legal :: Term -> Atom
legal t = Atom "legal" [ t ]

solve' :: Term -> Term -> Term -> Atom
solve' st history sts = Atom "solve'" [ st, history, sts ]

solve :: Term -> Atom
solve sts = Atom "solve" [ sts ]

program :: [ Clause ]
program =
  List.defs <>
  -- Moves in one direction
  [ fact $ move
      (state
        (bank farmer wolf "?C" "?G") (bank vacant vacant "?C'" "?G'"))
      (state
        (bank vacant vacant "?C" "?G") (bank farmer wolf "?C'" "?G'"))
  , fact $ move
      (state
        (bank farmer "?W" cabbage "?G") (bank vacant "?W'" vacant "?G'"))
      (state
        (bank vacant "?W" vacant "?G") (bank farmer "?W'" cabbage "?G'"))
  , fact $ move
      (state
        (bank farmer "?W" "?C" goat) (bank vacant "?W'" "?C'" vacant))
      (state
        (bank vacant "?W" "?C" vacant) (bank farmer "?W'" "?C'" goat))
  , fact $ move
      (state
        (bank farmer "?W" "?C" "?G") (bank vacant "?W'" "?C'" "?G'"))
      (state
        (bank vacant "?W" "?C" "?G") (bank farmer "?W'" "?C'" "?G'"))

  -- Moves back and forth
  , bimove "?St" "?St'" |- move "?St" "?St'"
  , bimove "?St" "?St'" |- move "?St'" "?St"

  -- Start and end states
  , fact $ start $ state (bank farmer wolf cabbage goat) (bank vacant vacant vacant vacant)
  , fact $ end $ state (bank vacant vacant vacant vacant) (bank farmer wolf cabbage goat) 

  -- Legal state
  , fact $ unsafe $ bank vacant wolf "?C" goat
  , fact $ unsafe $ bank vacant "?W" cabbage goat
  , fact $ safe $ bank farmer "?W" "?C" "?G"
  , safe "?B" |- neg (unsafe "?B")
  , legal (state "?B" "?B'") |- safe "?B" /\ safe "?B'"

  -- Solver
  , solve' "?St" "?History" "?History" |- end "?St"
  , solve' "?St" "?History" "?Sts" |-
      bimove "?St" "?St'" /\
      neg (List.member "?St'" "?History") /\
      legal "?St'" /\
      solve' "?St'" (List.cons "?St'" "?History") "?Sts"
  , solve "?Sts" |-
      start "?St" /\
      solve' "?St" (List.cons "?St" List.nil) "?Sts"
  ]
