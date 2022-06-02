module Language.Protolog.Substitution where

import qualified Data.Partition as P

import Language.Protolog.AST
import Language.Protolog.Unification

substClause :: Env -> Clause -> Clause
substClause env (head :- body) = substAtom env head :- (substAtom env <$> body)

substAtom :: Env -> Atom -> Atom
substAtom env (Atom name terms) = Atom name (substTerms env terms)

substTerms :: Env -> [ Term ] -> [ Term ]
substTerms env = map (substTerm env)

substTerm :: Env -> Term -> Term
substTerm env t@Var{}
  | rep == rep' = rep
  | otherwise = substTerm env rep'
  where
  rep = P.rep env t
  rep' = substTerm env rep
substTerm env (Fx name terms) = Fx name (substTerms env terms)
