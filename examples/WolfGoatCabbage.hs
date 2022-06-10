{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
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

program :: ProtologM (Term -> Atom)
program = do
  List.include
  -- Moves in one direction
  move <- freshPred @2
  move
    (state
      (bank farmer wolf "?C" "?G") (bank vacant vacant "?C'" "?G'"))
    (state
      (bank vacant vacant "?C" "?G") (bank farmer wolf "?C'" "?G'"))
    |- ()
  move
    (state
      (bank farmer "?W" cabbage "?G") (bank vacant "?W'" vacant "?G'"))
    (state
      (bank vacant "?W" vacant "?G") (bank farmer "?W'" cabbage "?G'"))
    |- ()
  move
    (state
      (bank farmer "?W" "?C" goat) (bank vacant "?W'" "?C'" vacant))
    (state
      (bank vacant "?W" "?C" vacant) (bank farmer "?W'" "?C'" goat))
    |- ()
  move
    (state
      (bank farmer "?W" "?C" "?G") (bank vacant "?W'" "?C'" "?G'"))
    (state
      (bank vacant "?W" "?C" "?G") (bank farmer "?W'" "?C'" "?G'"))
    |- ()

  -- Moves back and forth
  bimove <- freshPred @2
  bimove "?St" "?St'" |- move "?St" "?St'"
  bimove "?St" "?St'" |- move "?St'" "?St"

  -- Start and end states
  start <- freshPred @1
  start (state (bank farmer wolf cabbage goat) (bank vacant vacant vacant vacant)) |- ()

  end <- freshPred @1
  end (state (bank vacant vacant vacant vacant) (bank farmer wolf cabbage goat))  |- ()

  -- Legal state
  unsafe <- freshPred @1
  unsafe (bank vacant wolf "?C" goat) |- ()
  unsafe (bank vacant "?W" cabbage goat) |- ()

  safe <- freshPred @1
  safe (bank farmer "?W" "?C" "?G") |- ()
  safe "?B" |- neg (unsafe "?B")

  legal <- freshPred @1
  legal (state "?B" "?B'") |- safe "?B" /\ safe "?B'"

  -- Solver
  solve' <- freshPred @3
  solve' "?St" "?History" "?History" |- end "?St"
  solve' "?St" "?History" "?Sts" |-
    bimove "?St" "?St'" /\
    neg (List.member "?St'" "?History") /\
    legal "?St'" /\
    solve' "?St'" (List.cons "?St'" "?History") "?Sts"

  solve <- freshPred @1
  solve "?Sts" |-
    start "?St" /\
    solve' "?St" (List.cons "?St" List.nil) "?Sts"

  pure solve
