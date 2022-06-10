{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module TransitiveClosure where

import Language.Protolog

transitiveClosure :: (Term -> Term -> Atom) -> ProtologM (Term -> Term -> Atom)
transitiveClosure base = do
  tr <- freshPred @2
  tr "?X" "?Y" |- base "?X" "?Y"
  tr "?X" "?Z" |- base "?X" "?Y" /\ tr "?Y" "?Z"
  pure tr

program :: ProtologM (Term -> Term -> Atom)
program = do
  adviser <- freshPred @2
  adviser "Alan Turing" "Alonzo Church" |- ()
  adviser "Dana Scott" "Alonzo Church" |- ()
  adviser "Raymond Smullyan" "Alonzo Church" |- ()
  adviser "Alonzo Church" "Oswald Veblen" |- ()

  transitiveClosure adviser
