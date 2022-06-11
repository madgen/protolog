{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Language.Protolog.LinearResolutionSpec where

import qualified Data.Partition as P

import Test.Hspec

import Language.Protolog.AST
import Language.Protolog.LinearResolution
import Language.Protolog.Provenance
import Language.Protolog.DSL

import Common

spec :: Spec
spec =
  describe "LinearResolution" $ do
    describe "p :- q, r. q :- r. r." $ do
      let ((p, q, r, s), pr) = generate $ do
            p <- freshPred @0
            q <- freshPred @0
            r <- freshPred @0
            s <- freshPred @0

            p |- q /\ r
            q |- r
            r |- ()

            pure (p, q, r, s)

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
      let ((p, q, r, s), pr) = generate $ do
            p <- freshPred @0
            q <- freshPred @0
            r <- freshPred @0
            s <- freshPred @0

            p |- q /\ r
            q |- s
            q |- r
            r |- ()

            pure (p,q,r,s)

      it "p" $ holds pr p
      it "q" $ holds pr q
      it "r" $ holds pr r
      it "not s" $ doesntHold pr s
      it "neg s" $ holds pr (neg s)

    describe "p :- q, r. q :- s. q :- r." $ do
      let ((p, q, r, s), pr) = generate $ do
            p <- freshPred @0
            q <- freshPred @0
            r <- freshPred @0
            s <- freshPred @0

            p |- q /\ r
            q |- s
            q |- r

            pure (p,q,r,s)

      it "not p" $ doesntHold pr p
      it "not q" $ doesntHold pr q
      it "not r" $ doesntHold pr r
      it "not s" $ doesntHold pr s

    describe "p :- q. q. q :- q. (non-terminating if backtracks)" $ do
      let ((p, q), pr) = generate $ do
            p <- freshPred @0
            q <- freshPred @0

            p |- q
            q |- ()
            q |- q

            pure (p, q)

      it "p" $ holds pr p
      it "q" $ holds pr q

    describe "reflexive" $ do
      let (refl, pr) = generate $ do
            refl <- freshPred @2
            refl "?X" "?X" |- ()
            pure refl

      let resolvesTo' = resolvesTo pr
      it "refl(1, ?X) resolves to refl(1, 1)" $
        refl "1" "?X" `resolvesTo'` refl "1" "1"

      it "refl(?X, 1) resolves to refl(1, 1)" $
        refl "?X" "1" `resolvesTo'` refl "1" "1"

      it "not refl(1, 2)" $ doesntHold pr $ refl "1" "2"

      it "neg refl(1, 2)" $ holds pr $ neg $ refl "1" "2"

      it "refl(?X, ?Y)" $ holds pr $ refl "?X" "?Y"

    describe "ancestor" $ do
      let (ancestor, pr) = generate $ do
            adviser <- freshPred @2
            adviser "Andy Rice" "Mistral Contrastin" |- ()
            adviser "Dominic Orchard" "Mistral Contrastin" |- ()
            adviser "Alan Mycroft" "Dominic Orchard" |- ()
            adviser "Andy Hopper" "Andy Rice" |- ()
            adviser "David Wheeler" "Andy Hopper" |- ()

            ancestor <- freshPred @2
            ancestor "?X" "?Y" |- adviser "?X" "?Y"
            ancestor "?X" "?Z" |- adviser "?X" "?Y" /\ ancestor "?Y" "?Z"

            pure ancestor

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
      let ((z, succ, even), pr) = generate $ do
            -- Natural numbers
            z <- freshFx @0
            succ <- freshFx @1

            even <- freshPred @1
            even z |- ()
            even (succ $ succ "?N") |- even "?N"
            pure (z, succ, even)

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
        even (succ $ succ z)

      it "even(succ(succ(succ(?N)))) resolves N to succ(z)" $
        even (succ $ succ $ succ "?N") `resolvesTo'`
        even (succ $ succ $ succ $ succ z)

    describe "flying birds" $ do
      let (fly, pr) = generate $ do
            robin <- freshPred @1
            robin "Henri" |- ()
            kiwi <- freshPred @1
            kiwi "Mistral" |- ()
            owl <- freshPred @1
            owl "Michael" |- ()

            bird <- freshPred @1
            bird "?B" |- robin "?B"
            bird "?B" |- owl "?B"
            bird "?B" |- kiwi "?B"

            no_fly_list <- freshPred @1
            no_fly_list "?SadBird" |- kiwi "?SadBird"

            fly <- freshPred @1
            fly "?B" |- bird "?B" /\ neg (no_fly_list "?B")

            pure fly

      it "fly('Henri')" $
        holds pr (fly "Henri")

      it "fly('Michael')" $
        holds pr (fly "Michael")

      it "not fly('Mistral')" $
        doesntHold pr (fly "Mistral")
