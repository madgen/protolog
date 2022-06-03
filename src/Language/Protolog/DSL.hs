{-# LANGUAGE FlexibleInstances #-}

module Language.Protolog.DSL where

import Language.Protolog.AST

infix 5 |-
(|-) :: InBody body => Atom -> body -> Clause
head |- body = head :- raise body

infixr 6 /\
(/\) :: InBody a => InBody b => a -> b -> [ Literal ]
a /\ b = raise a <> raise b

class InBody a where
  raise :: a -> [ Literal ]

instance InBody Atom where
  raise atom = [ Literal Positive atom ]

instance InBody Literal where
  raise lit = [ lit ]

instance InBody [ Literal ] where
  raise lits = lits

class Negatable a where
  neg :: a -> Literal

instance Negatable Atom where
  neg = Literal Negative

instance Negatable Literal where
  neg (Literal Positive atom) = Literal Negative atom
  neg (Literal Negative atom) = error "No support for double negation"

fact :: Atom -> Clause
fact atom = atom :- []
