module LinearResolution where

import qualified Data.Partition as P

import AST
import Unification
import Naming

type GoalStack = [ [ Atom ] ]

resolve :: Env -> GoalStack -> Clause -> Maybe (Env, GoalStack)
resolve _ [] _ = error "Why are you resolving?"
resolve _ ([] : _) _ = error "Why are you resolving?"
resolve env ((p : ps) : pss) (q :- qs)
  | Just env <- unify env p q = Just (env, qs : ps : pss)
  | otherwise = Nothing

derive :: [ Clause ] -> Atom -> Maybe Atom
derive originalClauses query =
    case go 0 P.empty [ [ query ] ] Nothing originalClauses of
      Just env -> Just (subst env query)
      Nothing -> Nothing
  where
  go :: Int -> Env -> GoalStack -> Maybe Clause -> [ Clause ] -> Maybe Env
  go _ env [] _ _ = Just env
  go i env ([] : goals) active clauses = go i env goals active clauses
  go i env goals Nothing (clause : clauses) = go i env goals (Just clause) clauses
  go i env goals (Just clause) clauses =
    case resolve env goals (nameClause i clause) of
      Just (env', goals')
        | Just env'' <- go (i + 1) env' goals' Nothing originalClauses ->
          Just env''
      _ -> go i env goals Nothing clauses
  go _ _ _ _ [] = Nothing

subst :: Env -> Atom -> Atom
subst env (Atom name terms) = Atom name (map (P.rep env) terms)
