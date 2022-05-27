{-# LANGUAGE OverloadedStrings #-}

module LinearResolutionSpec where

import AST
import LinearResolution

import Test.Hspec

spec :: Spec
spec =
  describe "LinearResolution" $ do
    describe "p :- q, r. q :- r. r." $ do
      let pr = [ "p" :- ["q", "r"], "q" :- [ "r" ], "r" :- [] ]
      let pred = derive pr
      it "p" $ "p" `shouldSatisfy` pred
      it "q" $ "q" `shouldSatisfy` pred
      it "r" $ "r" `shouldSatisfy` pred
      it "s" $ "s" `shouldNotSatisfy` pred

    describe "p :- q, r. q :- s. q :- r. r." $ do
      let pr = [ "p" :- ["q", "r"], "q" :- [ "s" ], "q" :- [ "r" ], "r" :- [] ]
      let pred = derive pr
      it "p" $ "p" `shouldSatisfy` pred
      it "q" $ "q" `shouldSatisfy` pred
      it "r" $ "r" `shouldSatisfy` pred
      it "s" $ "s" `shouldNotSatisfy` pred

    describe "p :- q, r. q :- s. q :- r." $ do
      let pr = [ "p" :- ["q", "r"], "q" :- [ "s" ], "q" :- [ "r" ] ]
      let pred = derive pr
      it "p" $ "p" `shouldNotSatisfy` pred
      it "q" $ "q" `shouldNotSatisfy` pred
      it "r" $ "r" `shouldNotSatisfy` pred
      it "s" $ "s" `shouldNotSatisfy` pred
