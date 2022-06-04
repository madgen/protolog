{-# LANGUAGE OverloadedStrings #-}
module Language.Protolog.StdLib.List where

import Prelude hiding (elem)

import Language.Protolog.AST
import Language.Protolog.DSL

nil :: Term
nil = Fx "List.nil" []

cons :: Term -> Term -> Term
cons x xs = Fx "List.cons" [ x, xs ]

member :: Term -> Term -> Atom
member x xs = Atom "List.member" [ x, xs ]

defs :: [ Clause ]
defs =
  -- `member(?X, ?XS)` holds when `?X` is a member of `?XS`
  [ fact $ member "?X" (cons "?X" "?XS")
  , member "?X" (cons "?Y" "?XS") |- member "?X" "?XS"
  ]
