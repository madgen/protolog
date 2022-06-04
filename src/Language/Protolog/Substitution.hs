module Language.Protolog.Substitution
  ( mk
  , isTrivial
  , clause
  , literal
  , atom
  , term
  , Substitution(..)
  ) where

import qualified Data.Partition as P

import Language.Protolog.AST
import qualified Language.Protolog.Unification as Unify

data Substitution = Substitution Term Term

instance Show Substitution where
  show (Substitution a b) = "[" <> show a <> "/" <> show b <> "]"

mk :: Unify.Env -> Term -> Substitution
mk env term = Substitution (P.rep env term) term

isTrivial :: Substitution -> Bool
isTrivial (Substitution a b) = a == b

clause :: Unify.Env -> Clause -> Clause
clause env (head :- body) =
  atom env head :- (literal env <$> body)

literal :: Unify.Env -> Literal -> Literal
literal env (Literal polarity a) = Literal polarity (atom env a)

atom :: Unify.Env -> Atom -> Atom
atom env (Atom name ts) = Atom name (terms env ts)

terms :: Unify.Env -> [ Term ] -> [ Term ]
terms env = map (term env)

term :: Unify.Env -> Term -> Term
term env t@Var{}
  | t == rep = t
  | otherwise = term env rep
  where
  rep = P.rep env t
term env (Fx name ts) = Fx name (terms env ts)
