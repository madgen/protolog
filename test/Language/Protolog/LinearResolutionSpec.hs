{-# LANGUAGE OverloadedStrings #-}

module Language.Protolog.LinearResolutionSpec where

import Data.Maybe (isJust)

import Test.Hspec

import Language.Protolog.AST
import Language.Protolog.LinearResolution
import Language.Protolog.Provenance

lit :: Atom -> Literal
lit = Literal Positive

neg :: Literal -> Literal
neg (Literal Positive atom) = Literal Negative atom
neg (Literal Negative atom) = Literal Positive atom

spec :: Spec
spec =
  describe "LinearResolution" $ do
    describe "p :- q, r. q :- r. r." $ do
      let pr = [ "p" :- [lit "q", lit "r"], "q" :- [ lit "r" ], "r" :- [] ]
      let pred = isJust . run pr
      it "p" $ "p" `shouldSatisfy` pred
      it "q" $ "q" `shouldSatisfy` pred
      it "r" $ "r" `shouldSatisfy` pred
      it "s fails" $ "s" `shouldNotSatisfy` pred
      it "neg s" $ neg "s" `shouldSatisfy` pred

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
      let pr =
            [ "p" :- [ lit "q", lit "r" ]
            , "q" :- [ lit "s" ]
            , "q" :- [ lit "r" ]
            , "r" :- []
            ]
      let pred = isJust . run pr
      it "p" $ "p" `shouldSatisfy` pred
      it "q" $ "q" `shouldSatisfy` pred
      it "r" $ "r" `shouldSatisfy` pred
      it "not s" $ "s" `shouldNotSatisfy` pred
      it "neg s" $ neg "s" `shouldSatisfy` pred

    describe "p :- q, r. q :- s. q :- r." $ do
      let pr =
            [ "p" :- [ lit "q", lit "r" ]
            , "q" :- [ lit "s" ]
            , "q" :- [ lit "r" ]
            ]
      let pred = isJust . run pr
      it "not p" $ "p" `shouldNotSatisfy` pred
      it "not q" $ "q" `shouldNotSatisfy` pred
      it "not r" $ "r" `shouldNotSatisfy` pred
      it "not s" $ "s" `shouldNotSatisfy` pred

    describe "p :- q. q. q :- q. (non-terminating if backtracks)" $ do
      let pr =
            [ "p" :- [ lit "q" ]
            , "q" :- []
            , "q" :- [ lit "q" ]
            ]
      let pred = isJust . run pr
      it "p" $ "p" `shouldSatisfy` pred
      it "q" $ "q" `shouldSatisfy` pred

    describe "reflexive" $ do
      let refl t1 t2 = Atom "refl" [ t1, t2 ]
      let pr = [ refl "?X" "?X" :- [] ]

      let pred = run pr
      it "refl(1, ?X) resolves to refl(1, 1)" $
        pred (lit $ refl "1" "?X") `shouldBe` Just (lit $ refl "1" "1")

      it "refl(?X, 1) resolves to refl(1, 1)" $
        pred (lit $ refl "?X" "1") `shouldBe` Just (lit $ refl "1" "1")

      let pred = isJust . run pr
      it "not refl(1, 2)" $
        lit (refl "1" "2") `shouldNotSatisfy` pred

      it "not refl(1, 2)" $
        lit (refl "1" "2") `shouldNotSatisfy` pred

      it "neg refl(1, 2)" $
        neg (lit $ refl "1" "2") `shouldSatisfy` pred

      it "neg refl(1, 2)" $
        neg (lit $ refl "1" "2") `shouldSatisfy` pred

      it "refl(?X, ?Y)" $
        lit (refl "?X" "?Y") `shouldSatisfy` pred

    describe "ancestor" $ do
      let adviser t1 t2 = Atom "adviser" [t1, t2]
      let ancestor t1 t2 = Atom "ancestor" [t1, t2]
      let pr =
            [ ancestor "?X" "?Y" :- [ lit $ adviser "?X" "?Y" ]
            , ancestor "?X" "?Z" :- [ lit $ adviser "?X" "?Y", lit $ ancestor "?Y" "?Z" ]
            , adviser "Andy Rice" "Mistral Contrastin" :- []
            , adviser "Dominic Orchard" "Mistral Contrastin" :- []
            , adviser "Alan Mycroft" "Dominic Orchard" :- []
            , adviser "Andy Hopper" "Andy Rice" :- []
            , adviser "David Wheeler" "Andy Hopper" :- []
            ]
      let pred = isJust . run pr
      it "ancestor('Andy Rice', 'Mistral Contrastin')" $
        lit (ancestor "Andy Rice" "Mistral Contrastin") `shouldSatisfy` pred

      it "ancestor('David Wheeler', 'Mistral Contrastin')" $
        lit(ancestor "David Wheeler" "Mistral Contrastin") `shouldSatisfy` pred

      it "ancestor('Alan Mycroft', 'Mistral Contrastin')" $
        lit (ancestor "Alan Mycroft" "Mistral Contrastin") `shouldSatisfy` pred

      it "not ancestor('Alan Mycroft', 'Andrew Rice')" $
        lit (ancestor "Alan Mycroft" "Andrew Rice") `shouldNotSatisfy` pred

      it "not ancestor('Alan Mycroft', 'Alan Mycroft')" $
        lit (ancestor "Alan Mycroft" "Alan Mycroft") `shouldNotSatisfy` pred

      it "neg ancestor('Alan Mycroft', 'Andrew Rice')" $
        neg (lit $ ancestor "Alan Mycroft" "Andrew Rice") `shouldSatisfy` pred

      it "neg ancestor('Alan Mycroft', 'Alan Mycroft')" $
        neg (lit $ ancestor "Alan Mycroft" "Alan Mycroft") `shouldSatisfy` pred

      let pred = run pr
      it "ancestor('Andy Rice, ?X)" $
        pred (lit $ ancestor "Andy Rice" "?X") `shouldBe`
        Just (lit $ ancestor "Andy Rice" "Mistral Contrastin")

    describe "even" $ do
      let even t = Atom "even" [ t ]
      let succ t = Fx "succ" [ t ]
      let z = "z"
      let pr =
            [ even z :- []
            , even (succ $ succ "?N") :- [ lit $ even "?N" ]
            ]

      let pred = isJust . run pr
      it "even(z)" $
        lit (even z) `shouldSatisfy` pred

      it "not even(succ(z))" $
        lit (even $ succ z) `shouldNotSatisfy` pred

      it "neg even(succ(z))" $
        neg (lit $ even $ succ z) `shouldSatisfy` pred

      it "even(succ(succ(z)))" $
        lit (even $ succ $ succ z) `shouldSatisfy` pred

      it "not even(succ(succ(succ(z))))" $
        lit (even $ succ $ succ $ succ z) `shouldNotSatisfy` pred

      it "neg even(succ(succ(succ(z))))" $
        neg (lit $ even $ succ $ succ $ succ z) `shouldSatisfy` pred

      it "even(succ(succ(succ(succ(z)))))" $
        lit (even $ succ $ succ $ succ $ succ z) `shouldSatisfy` pred

      let pred = run pr
      it "even((succ(?N)) resolves N to succ(z)" $
        pred (lit $ even (succ "?N")) `shouldBe`
        Just (lit $ even (succ $ succ "z"))

      it "even(succ(succ(succ(?N)))) resolves N to succ(z)" $
        pred (lit $ even $ succ $ succ $ succ "?N") `shouldBe`
        Just (lit $ even $ succ $ succ $ succ $ succ "z")

    describe "flying birds" $ do
      let robin t = Atom "robin" [ t ]
      let owl t = Atom "owl" [ t ]
      let kiwi t = Atom "kiwi" [ t ]
      let bird t = Atom "bird" [ t ]
      let fly t = Atom "fly" [ t ]
      let no_fly_list  t = Atom "no_fly_list" [ t ]
      let pr =
            [ fly "?B" :- [ lit $ bird "?B", neg $ lit $ no_fly_list "?B" ]
            , bird "?B" :- [ lit $ robin "?B" ]
            , bird "?B" :- [ lit $ owl "?B" ]
            , bird "?B" :- [ lit $ kiwi "?B" ]
            , no_fly_list "?SadBird" :- [ lit $ kiwi "?SadBird" ]
            , robin "Henri" :- []
            , kiwi "Mistral" :- []
            , owl "Michael" :- []
            ]

      let pred = isJust . run pr
      it "fly('Henri')" $
        lit (fly "Henri") `shouldSatisfy` pred

      it "fly('Michael')" $
        lit (fly "Michael") `shouldSatisfy` pred

      it "not fly('Mistral')" $
        lit (fly "Mistral") `shouldNotSatisfy` pred
