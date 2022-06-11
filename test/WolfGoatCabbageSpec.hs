{-# LANGUAGE OverloadedStrings #-}

module WolfGoatCabbageSpec where

import Data.Maybe (isJust)

import Test.Hspec

import Language.Protolog.AST
import Language.Protolog.DSL
import Language.Protolog.LinearResolution
import WolfGoatCabbage

lit :: Atom -> Literal
lit = Literal Positive

class Holds a where
  holds :: [ Clause ] -> a -> Expectation
  doesntHold :: [ Clause ] -> a -> Expectation
  resolvesTo :: [ Clause ] -> a -> a -> Expectation

instance Holds Atom where
  holds pr atom = lit atom `shouldSatisfy` isJust . run pr
  doesntHold pr atom = lit atom `shouldNotSatisfy` isJust . run pr
  resolvesTo pr atom atom' = run pr (lit atom) `shouldBe` Just (lit atom')

instance Holds Literal where
  holds pr lit = lit `shouldSatisfy` isJust . run pr
  doesntHold pr lit = lit `shouldNotSatisfy` isJust . run pr
  resolvesTo pr lit lit' = run pr lit `shouldBe` Just lit'

spec :: Spec
spec =
  it "solves correctly" $ do
    let (solve, pr) = generate program
    holds pr (solve "?St")
