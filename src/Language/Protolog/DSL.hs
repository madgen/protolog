{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

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

class KnownNat n => CanPredicate n fx | n -> fx where
  mkPred :: Name -> fx
  namePred :: Name -> fx -> fx

instance CanPredicate 0 Atom where
  mkPred name = Atom
    { _canonicalName = name
    , _printName = name
    , _terms = []
    }
  namePred = namePred_

instance CanPredicate 1 (Term -> Atom) where
  mkPred name t = Atom
    { _canonicalName = name
    , _printName = name
    , _terms = [ t ]
    }
  namePred name = fmap (namePred_ name)

instance CanPredicate 2 (Term -> Term -> Atom) where
  mkPred name t1 t2 = Atom
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2 ]
    }
  namePred name = fmap (fmap (namePred_ name))

instance CanPredicate 3 (Term -> Term -> Term -> Atom) where
  mkPred name t1 t2 t3 = Atom
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2, t3 ]
    }
  namePred name = fmap (fmap (fmap (namePred_ name)))

instance CanPredicate 4 (Term -> Term -> Term -> Term -> Atom) where
  mkPred name t1 t2 t3 t4 = Atom
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2, t3, t4 ]
    }
  namePred name = fmap (fmap (fmap (fmap (namePred_ name))))

instance CanPredicate 5 (Term -> Term -> Term -> Term -> Term -> Atom) where
  mkPred name t1 t2 t3 t4 t5 = Atom
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2, t3, t4, t5 ]
    }
  namePred name = fmap (fmap (fmap (fmap (fmap (namePred_ name)))))

instance CanPredicate 6 (Term -> Term -> Term -> Term -> Term -> Term -> Atom) where
  mkPred name t1 t2 t3 t4 t5 t6 = Atom
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2, t3, t4, t5, t6 ]
    }
  namePred name = fmap (fmap (fmap (fmap (fmap (fmap (namePred_ name))))))

instance CanPredicate 7 (Term -> Term -> Term -> Term -> Term -> Term -> Term -> Atom) where
  mkPred name t1 t2 t3 t4 t5 t6 t7 = Atom
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2, t3, t4, t5, t6, t7 ]
    }
  namePred name = fmap (fmap (fmap (fmap (fmap (fmap (fmap (namePred_ name)))))))

instance CanPredicate 8 (Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term -> Atom) where
  mkPred name t1 t2 t3 t4 t5 t6 t7 t8 = Atom
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2, t3, t4, t5, t6, t7, t8 ]
    }
  namePred name = fmap (fmap (fmap (fmap (fmap (fmap (fmap (fmap (namePred_ name))))))))

instance CanPredicate 9 (Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term -> Atom) where
  mkPred name t1 t2 t3 t4 t5 t6 t7 t8 t9 = Atom
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2, t3, t4, t5, t6, t7, t8, t9 ]
    }
  namePred name = fmap (fmap (fmap (fmap (fmap (fmap (fmap (fmap (fmap (namePred_ name)))))))))

namePred_ :: Name -> Atom -> Atom
namePred_ name Atom{..} = Atom{_printName = name, ..}

freshPred :: forall n fx. CanPredicate n fx => ProtologM fx
freshPred = do
  ctr <- _counter <$> St.get
  St.modify (\st -> st { _counter = ctr + 1 })
  pure $ mkPred @n (T.pack $ "_p" <> show ctr)

class KnownNat n => CanFunction n fx | n -> fx where
  mkFx :: Name -> fx
  nameFx :: Name -> fx -> fx

instance CanFunction 0 Term where
  mkFx name = Fx
    { _canonicalName = name
    , _printName = name
    , _terms = []
    }
  nameFx = nameFx_

instance CanFunction 1 (Term -> Term) where
  mkFx name t = Fx
    { _canonicalName = name
    , _printName = name
    , _terms = [ t ]
    }
  nameFx name = fmap (nameFx_ name)

instance CanFunction 2 (Term -> Term -> Term) where
  mkFx name t1 t2 = Fx
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2 ]
    }
  nameFx name = fmap (fmap (nameFx_ name))

instance CanFunction 3 (Term -> Term -> Term -> Term) where
  mkFx name t1 t2 t3 = Fx
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2, t3 ]
    }
  nameFx name = fmap (fmap (fmap (nameFx_ name)))

instance CanFunction 4 (Term -> Term -> Term -> Term -> Term) where
  mkFx name t1 t2 t3 t4 = Fx
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2, t3, t4 ]
    }
  nameFx name = fmap (fmap (fmap (fmap (nameFx_ name))))

instance CanFunction 5 (Term -> Term -> Term -> Term -> Term -> Term) where
  mkFx name t1 t2 t3 t4 t5 = Fx
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2, t3, t4, t5 ]
    }
  nameFx name = fmap (fmap (fmap (fmap (fmap (nameFx_ name)))))

instance CanFunction 6 (Term -> Term -> Term -> Term -> Term -> Term -> Term) where
  mkFx name t1 t2 t3 t4 t5 t6 = Fx
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2, t3, t4, t5, t6 ]
    }
  nameFx name = fmap (fmap (fmap (fmap (fmap (fmap (nameFx_ name))))))

instance CanFunction 7 (Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term) where
  mkFx name t1 t2 t3 t4 t5 t6 t7 = Fx
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2, t3, t4, t5, t6, t7 ]
    }
  nameFx name = fmap (fmap (fmap (fmap (fmap (fmap (fmap (nameFx_ name)))))))

instance CanFunction 8 (Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term) where
  mkFx name t1 t2 t3 t4 t5 t6 t7 t8 = Fx
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2, t3, t4, t5, t6, t7, t8 ]
    }
  nameFx name = fmap (fmap (fmap (fmap (fmap (fmap (fmap (fmap (nameFx_ name))))))))

instance CanFunction 9 (Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term -> Term) where
  mkFx name t1 t2 t3 t4 t5 t6 t7 t8 t9 = Fx
    { _canonicalName = name
    , _printName = name
    , _terms = [ t1, t2, t3, t4, t5, t6, t7, t8, t9 ]
    }
  nameFx name = fmap (fmap (fmap (fmap (fmap (fmap (fmap (fmap (fmap (nameFx_ name)))))))))

nameFx_ :: Name -> Term -> Term
nameFx_ name Fx{..} = Fx{_printName = name, ..}
nameFx_ _ Var{} = error "cannot name a variable"

freshFx :: forall n fx. CanFunction n fx => ProtologM fx
freshFx = do
  ctr <- _counter <$> St.get
  St.modify (\st -> st { _counter = ctr + 1 })
  pure $ mkFx @n (T.pack $ "_f" <> show ctr)
