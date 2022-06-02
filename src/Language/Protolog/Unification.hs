module Language.Protolog.Unification (Env, unify) where

import qualified Data.Partition as P

import Language.Protolog.AST

type Env = P.Partition Term

unify :: Env -> Atom -> Atom -> Maybe Env
unify env (Atom p ts) (Atom p' ts')
  | p == p' = unifyTerms env ts ts'
  | otherwise = Nothing

unifyTerms :: Env -> [ Term ] -> [ Term ] -> Maybe Env
unifyTerms env ts ts' = foldr unifyTerm (Just env) $ zip ts ts'

unifyTerm :: (Term, Term) -> Maybe Env -> Maybe Env
unifyTerm _ Nothing = Nothing
unifyTerm (t1, t2) (Just env) =
  case (P.rep env t1, P.rep env t2) of
    (t1'@(Var x), t2') -> Just (P.joinElems t1' t2' env)
    (t1', t2'@(Var x)) -> Just (P.joinElems t1' t2' env)
    (Fx c ts, Fx c' ts') ->
      if c == c' then unifyTerms env ts ts' else Nothing
