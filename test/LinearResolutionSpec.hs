{-# LANGUAGE OverloadedStrings #-}

module LinearResolutionSpec where

import Data.Maybe (isJust)

import Test.Hspec

import AST
import LinearResolution
import Provenance

spec :: Spec
spec =
  describe "LinearResolution" $ do
    describe "p :- q, r. q :- r. r." $ do
      let pr = [ "p" :- ["q", "r"], "q" :- [ "r" ], "r" :- [] ]
      let pred = isJust . run pr
      it "p" $ "p" `shouldSatisfy` pred
      it "q" $ "q" `shouldSatisfy` pred
      it "r" $ "r" `shouldSatisfy` pred
      it "not s" $ "s" `shouldNotSatisfy` pred

      let pred = runWithProvenance pr
      it "provenance of p" $
        pred "p" `shouldBe`
          Just
            (PNode
              (PNode
                (PNode
                  (PNode
                    (PLeaf ["p"])
                    ["q", "r"]
                    ("p" :- ["q", "r"]))
                  ["r", "r"]
                  ("q" :- ["r"]))
                ["r"]
                ("r" :- []))
              []
              ("r" :- []))

    describe "p :- q, r. q :- s. q :- r. r." $ do
      let pr = [ "p" :- ["q", "r"], "q" :- [ "s" ], "q" :- [ "r" ], "r" :- [] ]
      let pred = isJust . run pr
      it "p" $ "p" `shouldSatisfy` pred
      it "q" $ "q" `shouldSatisfy` pred
      it "r" $ "r" `shouldSatisfy` pred
      it "not s" $ "s" `shouldNotSatisfy` pred

    describe "p :- q, r. q :- s. q :- r." $ do
      let pr = [ "p" :- ["q", "r"], "q" :- [ "s" ], "q" :- [ "r" ] ]
      let pred = isJust . run pr
      it "not p" $ "p" `shouldNotSatisfy` pred
      it "not q" $ "q" `shouldNotSatisfy` pred
      it "not r" $ "r" `shouldNotSatisfy` pred
      it "not s" $ "s" `shouldNotSatisfy` pred

    describe "reflexive" $ do
      let refl t1 t2 = Atom "refl" [ t1, t2 ]
      let pr = [ refl "?X" "?X" :- [] ]

      let pred = run pr
      it "refl(1, ?X) resolves to refl(1, 1)" $
        pred (refl "1" "?X") `shouldBe` Just (refl "1" "1")

      it "refl(?X, 1) resolves to refl(1, 1)" $
        pred (refl "?X" "1") `shouldBe` Just (refl "1" "1")

      it "not refl(1, 2)" $
        pred (refl "1" "2") `shouldBe` Nothing

      it "refl(?X, ?Y)" $
        refl "?X" "?Y" `shouldSatisfy` isJust . run pr

    describe "ancestor" $ do
      let adviser t1 t2 = Atom "adviser" [t1, t2]
      let ancestor t1 t2 = Atom "ancestor" [t1, t2]
      let pr =
            [ ancestor "?X" "?Y" :- [ adviser "?X" "?Y" ]
            , ancestor "?X" "?Z" :- [ adviser "?X" "?Y", ancestor "?Y" "?Z" ]
            , adviser "Andy Rice" "Mistral Contrastin" :- []
            , adviser "Dominic Orchard" "Mistral Contrastin" :- []
            , adviser "Alan Mycroft" "Dominic Orchard" :- []
            , adviser "Andy Hopper" "Andy Rice" :- []
            , adviser "David Wheeler" "Andy Hopper" :- []
            ]
      let pred = isJust . run pr
      it "ancestor('Andy Rice', 'Mistral Contrastin')" $
        ancestor "Andy Rice" "Mistral Contrastin" `shouldSatisfy` pred
      it "ancestor('David Wheeler', 'Mistral Contrastin')" $
        ancestor "David Wheeler" "Mistral Contrastin" `shouldSatisfy` pred
      it "ancestor('Alan Mycroft', 'Mistral Contrastin')" $
        ancestor "Alan Mycroft" "Mistral Contrastin" `shouldSatisfy` pred
      it "not ancestor('Alan Mycroft', 'Andrew Rice')" $
        ancestor "Alan Mycroft" "Andrew Rice" `shouldNotSatisfy` pred
      it "not ancestor('Alan Mycroft', 'Alan Mycroft')" $
        ancestor "Alan Mycroft" "Alan Mycroft" `shouldNotSatisfy` pred

      let pred = run pr
      it "ancestor('Andy Rice, ?X)" $
        pred (ancestor "Andy Rice" "?X") `shouldBe`
        Just (ancestor "Andy Rice" "Mistral Contrastin")

    describe "even" $ do
      let even t = Atom "even" [ t ]
      let succ t = Fx "succ" [ t ]
      let z = "z"
      let pr =
            [ even z :- []
            , even (succ $ succ "?N") :- [ even "?N" ]
            ]

      let pred = isJust . run pr
      it "even(z)" $
        even z `shouldSatisfy` pred

      it "not even(succ(z))" $
        even (succ z) `shouldNotSatisfy` pred

      it "even(succ(succ(z)))" $
        even (succ $ succ z) `shouldSatisfy` pred

      it "not even(succ(succ(succ(z))))" $
        even (succ $ succ $ succ z) `shouldNotSatisfy` pred

      it "even(succ(succ(succ(succ(z)))))" $
        even (succ $ succ $ succ $ succ z) `shouldSatisfy` pred

      let pred = run pr
      it "even((succ(?N)) resolves N to succ(z)" $
        pred (even (succ "?N")) `shouldBe`
        Just (even (succ $ succ "z"))

      it "even(succ(succ(succ(?N)))) resolves N to succ(z)" $
        pred (even (succ $ succ $ succ "?N")) `shouldBe`
        Just (even (succ $ succ $ succ $ succ "z"))

