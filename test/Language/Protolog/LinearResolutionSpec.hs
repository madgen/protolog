{-# LANGUAGE OverloadedStrings #-}

module Language.Protolog.LinearResolutionSpec where

import Data.Maybe (isJust)
import qualified Data.Partition as P

import Test.Hspec

import Language.Protolog.AST
import Language.Protolog.LinearResolution
import Language.Protolog.Provenance
import Language.Protolog.DSL

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
  describe "LinearResolution" $ do
    describe "p :- q, r. q :- r. r." $ do
      let [p, q, r, s] = (`Atom` []) <$> ["p", "q", "r", "s"]
      let pr =
            [ p |- q /\ r
            , q |- r
            , fact r
            ]

      it "p" $ holds pr p
      it "q" $ holds pr q
      it "r" $ holds pr r
      it "s fails" $ doesntHold pr s
      it "neg s" $ holds pr $ neg s

      let pred = runWithProvenance pr
      it "provenance of p" $
        pred (lit p) `shouldBe`
          Just
            (PPositive
              (PPositive
                (PPositive
                  (PPositive
                    (PQuery [lit p])
                    P.empty
                    [lit q, lit r]
                    (p :- [lit q, lit r]))
                  P.empty
                  [lit r, lit r]
                  (q :- [ lit r]))
                P.empty
                [lit r]
                (r :- []))
              P.empty
              []
              (r :- []))

    describe "p :- q, r. q :- s. q :- r. r." $ do
      let [p, q, r, s] = (`Atom` []) <$> ["p", "q", "r", "s"]
      let pr =
            [ p |- q /\ r
            , q |- s
            , q |- r
            , fact r
            ]

      it "p" $ holds pr p
      it "q" $ holds pr q
      it "r" $ holds pr r
      it "not s" $ doesntHold pr s
      it "neg s" $ holds pr (neg s)

    describe "p :- q, r. q :- s. q :- r." $ do
      let [p, q, r, s] = (`Atom` []) <$> ["p", "q", "r", "s"]
      let pr =
            [ p |- q /\ r
            , q |- s
            , q |- r
            ]

      it "not p" $ doesntHold pr p
      it "not q" $ doesntHold pr q
      it "not r" $ doesntHold pr r
      it "not s" $ doesntHold pr s

    describe "p :- q. q. q :- q. (non-terminating if backtracks)" $ do
      let [p, q] = (`Atom` []) <$> ["p", "q"]
      let p = Atom "p" []
      let q = Atom "q" []
      let pr =
            [ p |- q
            , fact q
            , q |- q
            ]

      it "p" $ holds pr p
      it "q" $ holds pr q

    describe "reflexive" $ do
      let refl t1 t2 = Atom "refl" [ t1, t2 ]
      let pr = [ fact $ refl "?X" "?X" ]

      let resolvesTo' = resolvesTo pr
      it "refl(1, ?X) resolves to refl(1, 1)" $
        refl "1" "?X" `resolvesTo'` refl "1" "1"

      it "refl(?X, 1) resolves to refl(1, 1)" $
        refl "?X" "1" `resolvesTo'` refl "1" "1"

      it "not refl(1, 2)" $ doesntHold pr $ refl "1" "2"

      it "neg refl(1, 2)" $ holds pr $ neg $ refl "1" "2"

      it "refl(?X, ?Y)" $ holds pr $ refl "?X" "?Y"

    describe "ancestor" $ do
      let adviser t1 t2 = Atom "adviser" [t1, t2]
      let ancestor t1 t2 = Atom "ancestor" [t1, t2]
      let pr =
            [ ancestor "?X" "?Y" |- adviser "?X" "?Y"
            , ancestor "?X" "?Z" |- adviser "?X" "?Y" /\ ancestor "?Y" "?Z"
            , fact $ adviser "Andy Rice" "Mistral Contrastin"
            , fact $ adviser "Dominic Orchard" "Mistral Contrastin"
            , fact $ adviser "Alan Mycroft" "Dominic Orchard"
            , fact $ adviser "Andy Hopper" "Andy Rice"
            , fact $ adviser "David Wheeler" "Andy Hopper"
            ]

      it "ancestor('Andy Rice', 'Mistral Contrastin')" $
        holds pr $ ancestor "Andy Rice" "Mistral Contrastin"

      it "ancestor('David Wheeler', 'Mistral Contrastin')" $
        holds pr $ ancestor "David Wheeler" "Mistral Contrastin"

      it "ancestor('Alan Mycroft', 'Mistral Contrastin')" $
        holds pr $ ancestor "Alan Mycroft" "Mistral Contrastin"

      it "not ancestor('Alan Mycroft', 'Andrew Rice')" $
        doesntHold pr $ ancestor "Alan Mycroft" "Andrew Rice"

      it "not ancestor('Alan Mycroft', 'Alan Mycroft')" $
        doesntHold pr $ ancestor "Alan Mycroft" "Alan Mycroft"

      it "neg ancestor('Alan Mycroft', 'Andrew Rice')" $
        holds pr $ neg $ ancestor "Alan Mycroft" "Andrew Rice"

      it "neg ancestor('Alan Mycroft', 'Alan Mycroft')" $
        holds pr $ neg $ ancestor "Alan Mycroft" "Alan Mycroft"

      let resolvesTo' = resolvesTo pr
      it "ancestor('Andy Rice, ?X)" $
        ancestor "Andy Rice" "?X" `resolvesTo'`
        ancestor "Andy Rice" "Mistral Contrastin"

    describe "even" $ do
      let even t = Atom "even" [ t ]
      let succ t = Fx "succ" [ t ]
      let z = "z"
      let pr =
            [ fact $ even z
            , even (succ $ succ "?N") |- even "?N"
            ]

      it "even(z)" $ holds pr $ even z

      it "not even(succ(z))" $
        doesntHold pr $ even $ succ z

      it "neg even(succ(z))" $
        holds pr $ neg $ even $ succ z

      it "even(succ(succ(z)))" $
        holds pr $ even $ succ $ succ z

      it "not even(succ(succ(succ(z))))" $
        doesntHold pr $ even $ succ $ succ $ succ z

      it "neg even(succ(succ(succ(z))))" $
        holds pr $ neg $ even $ succ $ succ $ succ z

      it "even(succ(succ(succ(succ(z)))))" $
        holds pr $ even $ succ $ succ $ succ $ succ z

      let resolvesTo' = resolvesTo pr
      it "even((succ(?N)) resolves N to succ(z)" $
        even (succ "?N") `resolvesTo'`
        even (succ $ succ "z")

      it "even(succ(succ(succ(?N)))) resolves N to succ(z)" $
        even (succ $ succ $ succ "?N") `resolvesTo'`
        even (succ $ succ $ succ $ succ "z")

    describe "flying birds" $ do
      let [robin, owl, kiwi, bird, fly, no_fly_list] =
            (\name t -> Atom name [ t ]) <$>
              ["robin", "owl", "kiwi", "bird", "fly", "no_fly_list"]
      let pr =
            [ fly "?B" |- bird "?B" /\ neg (no_fly_list "?B")
            , bird "?B" |- robin "?B"
            , bird "?B" |- owl "?B"
            , bird "?B" |- kiwi "?B"
            , no_fly_list "?SadBird" |- kiwi "?SadBird"
            , fact $ robin "Henri"
            , fact $ kiwi "Mistral"
            , fact $ owl "Michael"
            ]

      it "fly('Henri')" $
        holds pr (fly "Henri")

      it "fly('Michael')" $
        holds pr (fly "Michael")

      it "not fly('Mistral')" $
        doesntHold pr (fly "Mistral")
