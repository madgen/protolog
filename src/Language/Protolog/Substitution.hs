module Language.Protolog.Substitution where

import qualified Data.Partition as P

import Language.Protolog.AST
import Language.Protolog.Unification

data Substitution = Substitution Term Term

instance Show Substitution where
  show (Substitution a b) = "[" <> show a <> "/" <> show b <> "]"

mkSubst :: Env -> Term -> Substitution
mkSubst env term = Substitution (P.rep env term) term

isTrivialSubst :: Substitution -> Bool
isTrivialSubst (Substitution a b) = a == b

substClause :: Env -> Clause -> Clause
substClause env (head :- body) =
  substAtom env head :- (substLiteral env <$> body)

substLiteral :: Env -> Literal -> Literal
substLiteral env (Literal polarity atom) = Literal polarity (substAtom env atom)

substAtom :: Env -> Atom -> Atom
substAtom env (Atom name terms) = Atom name (substTerms env terms)

substTerms :: Env -> [ Term ] -> [ Term ]
substTerms env = map (substTerm env)

substTerm :: Env -> Term -> Term
substTerm env t@Var{}
  | t == rep = t
  | otherwise = substTerm env rep
  where
  rep = P.rep env t
substTerm env (Fx name terms) = Fx name (substTerms env terms)
