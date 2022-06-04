{-# LANGUAGE OverloadedStrings #-}
module TransitiveClosure where

import Language.Protolog

transitiveClosure :: (Term -> Term -> Atom) -> (Term -> Term -> Atom) -> [ Clause ]
transitiveClosure tr base =
  [ tr "?X" "?Y" |- base "?X" "?Y"
  , tr "?X" "?Z" |- base "?X" "?Y" /\ tr "?Y" "?Z"
  ]

adviser :: Term -> Term -> Atom
adviser t1 t2 = Atom "adviser" [ t1, t2 ]

academicAncestor :: Term -> Term -> Atom
academicAncestor t1 t2 = Atom "academicAncestor" [ t1, t2 ]

program :: [ Clause ]
program =
  transitiveClosure academicAncestor adviser <>
  [ fact $ adviser "Alan Turing" "Alonzo Church"
  , fact $ adviser "Dana Scott" "Alonzo Church"
  , fact $ adviser "Raymond Smullyan" "Alonzo Church"
  , fact $ adviser "Alonzo Church" "Oswald Veblen"
  ]
