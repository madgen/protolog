{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Protolog.StdLib.List where

import GHC.Exts (IsList(..))

import Language.Protolog.AST
import Language.Protolog.DSL

nil :: Term
nil = Fx "List.nil" "List.nil" []

cons :: Term -> Term -> Term
cons x xs = Fx "List.cons" "List.cons" [ x, xs ]

member :: Term -> Term -> Atom
member x xs = Atom "List.member" "List.member" [ x, xs ]

include :: ProtologM ()
include = do
  -- `member(?X, ?XS)` holds when `?X` is a member of `?XS`
  member "?X" (cons "?X" "?XS") |- ()
  member "?X" (cons "?Y" "?XS") |- member "?X" "?XS"

instance IsList Term where
  type Item Term = Term
  fromList [] = nil
  fromList (x : xs) = cons x (fromList xs)

  toList term
    | term == nil = []
    | Fx "List.cons" _ [ x, xs ] <- term = x : toList xs
    | otherwise = error "Trying to convert something that is not a list to a list" 
