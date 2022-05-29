module Naming (nameClause) where

import qualified Data.Text as T

import AST

nameClause :: Int -> Clause -> Clause
nameClause i (head :- body) = nameAtom i head :- map (nameAtom i) body

nameAtom :: Int -> Atom -> Atom
nameAtom i (Atom name terms) = Atom name $ map (nameTerm i) terms

nameTerm :: Int -> Term -> Term
nameTerm i t@Lit{} = t
nameTerm i (Var v) = Var $ T.pack (show i) <> v
