{-# LANGUAGE OverloadedStrings #-}

module WolfGoatCabbageSpec where

import Test.Hspec

import Language.Protolog.DSL
import Language.Protolog.StdLib.List ()
import WolfGoatCabbage

import Common
import GHC.Exts (IsList(fromList))
import Language.Protolog (Term(..))

stateFromLeftBank :: (Term -> Term -> Term)
                  -> (Term -> Term -> Term -> Term -> Term)
                  -> Term -> Term -> Term -> Term
                  -> Term
stateFromLeftBank state bank f w c g =
  state (bank f w c g) (bank (flipFarmer f) (flipWolf w) (flipCabbage c) (flipGoat g))
  where
  flipFarmer "vacant" = "farmer"
  flipFarmer "farmer" = "vacant"
  flipFarmer _ = error "unexpected term"

  flipWolf "vacant" = "wolf"
  flipWolf "wolf" = "vacant"
  flipWolf _ = error "unexpected term"

  flipGoat "vacant" = "goat"
  flipGoat "goat" = "vacant"
  flipGoat _ = error "unexpected term"

  flipCabbage "vacant" = "cabbage"
  flipCabbage "cabbage" = "vacant"
  flipCabbage _ = error "unexpected term"

spec :: Spec
spec =
  it "solves correctly" $ do
    let ((solve, state, bank), pr) = generate program
    let from = stateFromLeftBank state bank
    let moves =
          [ from "vacant" "vacant" "vacant" "vacant"
          , from "farmer" "vacant" "vacant" "goat"
          , from "vacant" "vacant" "vacant" "goat"
          , from "farmer" "vacant" "cabbage" "goat"
          , from "vacant" "vacant" "cabbage" "vacant"
          , from "farmer" "wolf" "cabbage" "vacant"
          , from "vacant" "wolf" "cabbage" "vacant"
          , from "farmer" "wolf" "cabbage" "goat"
          ]
    resolvesTo pr (solve "?St") (solve (fromList moves))
