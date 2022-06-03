{-# LANGUAGE OverloadedStrings #-}

module Language.Protolog.LinearResolutionSpec where

import Data.Maybe (isJust)

import Test.Hspec

import Language.Protolog.AST
import Language.Protolog.LinearResolution
import Language.Protolog.Provenance
import Language.Protolog.DSL

lit :: Atom -> Literal
lit = Literal Positive

spec :: Spec
spec =
  describe "LinearResolution" $ do
    describe "p :- q, r. q :- r. r." $ do
      let p = Atom "p" []
      let q = Atom "q" []
      let r = Atom "r" []
      let s = Atom "s" []
      let pr =
            [ p |- q /\ r
            , q |- r
            , fact r
            ]
      let pred = isJust . run pr
      it "p" $ lit p `shouldSatisfy` pred
      it "q" $ lit q `shouldSatisfy` pred
      it "r" $ lit r `shouldSatisfy` pred
      it "s fails" $ lit s `shouldNotSatisfy` pred
      it "neg s" $ neg s `shouldSatisfy` pred

      let pred = runWithProvenance pr
      it "provenance of p" $
        pred (lit p) `shouldBe`
          Just
            (PNode
              (PNode
                (PNode
                  (PNode
                    (PLeaf [lit p])
                    [lit q, lit r]
                    (p :- [lit q, lit r]))
                  [lit r, lit r]
                  (q :- [ lit r]))
                [lit r]
                (r :- []))
              []
              (r :- []))

    describe "p :- q, r. q :- s. q :- r. r." $ do
      let p = Atom "p" []
      let q = Atom "q" []
      let r = Atom "r" []
      let s = Atom "s" []
      let pr =
            [ p |- q /\ r
            , q |- s
            , q |- r
            , fact r
            ]
      let pred = isJust . run pr
      it "p" $ lit p `shouldSatisfy` pred
      it "q" $ lit q `shouldSatisfy` pred
      it "r" $ lit r `shouldSatisfy` pred
      it "not s" $ lit s `shouldNotSatisfy` pred
      it "neg s" $ neg s `shouldSatisfy` pred

    describe "p :- q, r. q :- s. q :- r." $ do
      let p = Atom "p" []
      let q = Atom "q" []
      let r = Atom "r" []
      let s = Atom "s" []
      let pr =
            [ p |- q /\ r
            , q |- s
            , q |- r
            ]
      let pred = isJust . run pr
      it "not p" $ lit p `shouldNotSatisfy` pred
      it "not q" $ lit q `shouldNotSatisfy` pred
      it "not r" $ lit r `shouldNotSatisfy` pred
      it "not s" $ lit s `shouldNotSatisfy` pred

    describe "p :- q. q. q :- q. (non-terminating if backtracks)" $ do
      let p = Atom "p" []
      let q = Atom "q" []
      let pr =
            [ p |- q
            , fact q
            , q |- q
            ]
      let pred = isJust . run pr
      it "p" $ lit p `shouldSatisfy` pred
      it "q" $ lit q `shouldSatisfy` pred

    describe "reflexive" $ do
      let refl t1 t2 = Atom "refl" [ t1, t2 ]
      let pr = [ fact $ refl "?X" "?X" ]

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
            [ ancestor "?X" "?Y" |- adviser "?X" "?Y"
            , ancestor "?X" "?Z" |- adviser "?X" "?Y" /\ ancestor "?Y" "?Z"
            , fact $ adviser "Andy Rice" "Mistral Contrastin"
            , fact $ adviser "Dominic Orchard" "Mistral Contrastin"
            , fact $ adviser "Alan Mycroft" "Dominic Orchard"
            , fact $ adviser "Andy Hopper" "Andy Rice"
            , fact $ adviser "David Wheeler" "Andy Hopper"
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
            [ fact $ even z
            , even (succ $ succ "?N") |- even "?N"
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
      let no_fly_list t = Atom "no_fly_list" [ t ]
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

      let pred = isJust . run pr
      it "fly('Henri')" $
        lit (fly "Henri") `shouldSatisfy` pred

      it "fly('Michael')" $
        lit (fly "Michael") `shouldSatisfy` pred

      it "not fly('Mistral')" $
        lit (fly "Mistral") `shouldNotSatisfy` pred
