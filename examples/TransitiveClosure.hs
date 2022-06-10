{-# LANGUAGE OverloadedStrings #-}
module TransitiveClosure where

import Language.Protolog

transitiveClosure :: (Term -> Term -> Atom) -> (Term -> Term -> Atom) -> ProtologM ()
transitiveClosure tr base = do
  tr "?X" "?Y" |- base "?X" "?Y"
  tr "?X" "?Z" |- base "?X" "?Y" /\ tr "?Y" "?Z"

adviser :: Term -> Term -> Atom
adviser t1 t2 = Atom "adviser" [ t1, t2 ]

academicAncestor :: Term -> Term -> Atom
academicAncestor t1 t2 = Atom "academicAncestor" [ t1, t2 ]

program :: ProtologM ()
program = do
  transitiveClosure academicAncestor adviser
  adviser "Alan Turing" "Alonzo Church" |- ()
  adviser "Dana Scott" "Alonzo Church" |- ()
  adviser "Raymond Smullyan" "Alonzo Church" |- ()
  adviser "Alonzo Church" "Oswald Veblen" |- ()
