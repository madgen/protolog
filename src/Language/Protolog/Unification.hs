module Language.Protolog.Unification
  ( Env
  , atom
  ) where

import qualified Data.Partition as P

import Language.Protolog.AST

type Env = P.Partition Term

atom :: Env -> Atom -> Atom -> Maybe Env
atom env (Atom p ts) (Atom p' ts')
  | p == p' = terms env ts ts'
  | otherwise = Nothing

terms :: Env -> [ Term ] -> [ Term ] -> Maybe Env
terms env ts ts' = foldr term (Just env) $ zip ts ts'

term :: (Term, Term) -> Maybe Env -> Maybe Env
term _ Nothing = Nothing
term (t1, t2) (Just env) =
  case (P.rep env t1, P.rep env t2) of
    (t1'@(Var x), t2') -> Just (P.joinElems t1' t2' env)
    (t1', t2'@(Var x)) -> Just (P.joinElems t1' t2' env)
    (Fx c ts, Fx c' ts') ->
      if c == c' then terms env ts ts' else Nothing
