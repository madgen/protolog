{-# LANGUAGE OverloadedStrings #-}

module WolfGoatCabbageSpec where

import Test.Hspec

import Language.Protolog.DSL
import WolfGoatCabbage

import Common

spec :: Spec
spec =
  it "solves correctly" $ do
    let (solve, pr) = generate program
    holds pr (solve "?St")
