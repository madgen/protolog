module Unification (Env, unify) where

import qualified Data.Partition as P

import AST

type Env = P.Partition Term

unify :: Env -> Atom -> Atom -> Maybe Env
unify env (Atom p ts) (Atom p' ts')
  | p == p' = foldr unifyTerm (Just env) $ zip ts ts'
  | otherwise = Nothing

unifyTerm :: (Term, Term) -> Maybe Env -> Maybe Env
unifyTerm _ Nothing = Nothing
unifyTerm (t1, t2) (Just env) =
  case (P.rep env t1, P.rep env t2) of
    (t1'@(Var x), t2') -> Just (P.joinElems t1' t2' env)
    (t1', t2'@(Var x)) -> Just (P.joinElems t1' t2' env)
    (Lit c, Lit c') -> if c == c' then Just env else Nothing
