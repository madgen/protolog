{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}

module Language.Protolog.DSL where

import qualified Control.Monad.Trans.State as St
import Control.Arrow (second)

import Language.Protolog.AST

type ProtologM a = St.State [ Clause ] a

generate :: ProtologM a -> (a, [ Clause ])
generate = second reverse . (`St.runState` [])

generate_ :: ProtologM a -> [ Clause ]
generate_ = snd . generate

infix 5 |-
(|-) :: InBody body => Atom -> body -> ProtologM ()
head |- body = St.modify (clause :)
  where
  clause = head :- raise body

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

instance InBody () where
  raise () = []

class Negatable a where
  neg :: a -> Literal

instance Negatable Atom where
  neg = Literal Negative

instance Negatable Literal where
  neg (Literal Positive atom) = Literal Negative atom
  neg (Literal Negative atom) = error "No support for double negation"
