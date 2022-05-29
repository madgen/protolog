{-# LANGUAGE OverloadedStrings #-}

module LinearResolutionSpec where

import Data.Maybe (isJust)

import Test.Hspec

import AST
import LinearResolution


spec :: Spec
spec =
  describe "LinearResolution" $ do
    describe "p :- q, r. q :- r. r." $ do
      let pr = [ "p" :- ["q", "r"], "q" :- [ "r" ], "r" :- [] ]
      let pred = isJust . derive pr
      it "p" $ "p" `shouldSatisfy` pred
      it "q" $ "q" `shouldSatisfy` pred
      it "r" $ "r" `shouldSatisfy` pred
      it "not s" $ "s" `shouldNotSatisfy` pred

    describe "p :- q, r. q :- s. q :- r. r." $ do
      let pr = [ "p" :- ["q", "r"], "q" :- [ "s" ], "q" :- [ "r" ], "r" :- [] ]
      let pred = isJust . derive pr
      it "p" $ "p" `shouldSatisfy` pred
      it "q" $ "q" `shouldSatisfy` pred
      it "r" $ "r" `shouldSatisfy` pred
      it "not s" $ "s" `shouldNotSatisfy` pred

    describe "p :- q, r. q :- s. q :- r." $ do
      let pr = [ "p" :- ["q", "r"], "q" :- [ "s" ], "q" :- [ "r" ] ]
      let pred = isJust . derive pr
      it "not p" $ "p" `shouldNotSatisfy` pred
      it "not q" $ "q" `shouldNotSatisfy` pred
      it "not r" $ "r" `shouldNotSatisfy` pred
      it "not s" $ "s" `shouldNotSatisfy` pred

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
      let pred = isJust . derive pr
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

      let pred = derive pr
      it "ancestor('Andy Rice, ?X)" $
        pred (ancestor "Andy Rice" "?X") `shouldBe` Just (ancestor "Andy Rice" "Mistral Contrastin")
