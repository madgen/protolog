{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

module Language.Protolog.DSL where

import qualified Control.Monad.Trans.State as St
import Control.Arrow (second)

import qualified Data.Text as T

import GHC.TypeLits

import Language.Protolog.AST

data ProtologSt = ProtologSt
  { _clauses :: [ Clause ]
  , _counter :: Int
  }

initSt :: ProtologSt
initSt = ProtologSt { _clauses = [], _counter = 0 }

type ProtologM a = St.State ProtologSt a

generate :: ProtologM a -> (a, [ Clause ])
generate = second (reverse . _clauses) . (`St.runState` initSt)

generate_ :: ProtologM a -> [ Clause ]
generate_ = snd . generate

infix 5 |-
(|-) :: InBody body => Atom -> body -> ProtologM ()
head |- body = St.modify (\st -> st { _clauses = clause : _clauses st })
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

class KnownNat n => Predicable n fx | n -> fx where
  mkPred :: T.Text -> fx

instance Predicable 0 Atom where
  mkPred name = Atom name []

instance Predicable 1 (Term -> Atom) where
  mkPred name t = Atom name [ t ]

instance Predicable 2 (Term -> Term -> Atom) where
  mkPred name t1 t2 = Atom name [ t1, t2 ]

instance Predicable 3 (Term -> Term -> Term -> Atom) where
  mkPred name t1 t2 t3 = Atom name [ t1, t2, t3 ]

instance Predicable 4 (Term -> Term -> Term -> Term -> Atom) where
  mkPred name t1 t2 t3 t4 = Atom name [ t1, t2, t3, t4 ]

instance Predicable 5 (Term -> Term -> Term -> Term -> Term -> Atom) where
  mkPred name t1 t2 t3 t4 t5 = Atom name [ t1, t2, t3, t4, t5 ]

instance Predicable 6 (Term -> Term -> Term -> Term -> Term -> Term -> Atom) where
  mkPred name t1 t2 t3 t4 t5 t6 = Atom name [ t1, t2, t3, t4, t5, t6 ]

instance Predicable 7 (Term -> Term -> Term -> Term -> Term -> Term -> Term -> Atom) where
  mkPred name t1 t2 t3 t4 t5 t6 t7 = Atom name [ t1, t2, t3, t4, t5, t6, t7 ]

instance Predicable 8 (Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term -> Atom) where
  mkPred name t1 t2 t3 t4 t5 t6 t7 t8 = Atom name [ t1, t2, t3, t4, t5, t6, t7, t8 ]

instance Predicable 9 (Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term -> Atom) where
  mkPred name t1 t2 t3 t4 t5 t6 t7 t8 t9 = Atom name [ t1, t2, t3, t4, t5, t6, t7, t8, t9 ]

freshPred :: forall n fx. Predicable n fx => ProtologM fx
freshPred = do
  ctr <- _counter <$> St.get
  St.modify (\st -> st { _counter = ctr + 1 })
  pure $ mkPred @n (T.pack $ "_p" <> show ctr)
