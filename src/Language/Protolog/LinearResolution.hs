{-# LANGUAGE ScopedTypeVariables #-}

module Language.Protolog.LinearResolution where

import qualified Data.Partition as P
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))

import Language.Protolog.AST
import Language.Protolog.Unification
import Language.Protolog.Naming
import Language.Protolog.Substitution
import Language.Protolog.Provenance

resolve :: Env -> GoalStack -> Clause -> Maybe (Env, GoalStack)
resolve _ [] _ = error "Why are you resolving?"
resolve _ ([] : _) _ = error "Why are you resolving?"
resolve env ((p : ps) : pss) (q :- qs)
  | Just env <- unify env p q = Just (env, qs : ps : pss)
  | otherwise = Nothing

derive :: forall p. Provenance p => [ Clause ] -> Atom -> Maybe (Env, p)
derive originalClauses query =
  go 0 P.empty queryGoalStack Nothing originalClauses queryProvenance
  where
  queryGoalStack = [ [ query ] ]
  queryProvenance = unit queryGoalStack

  go :: Int
     -> Env
     -> GoalStack
     -> Maybe Clause
     -> [ Clause ]
     -> p
     -> Maybe (Env, p)
  go _ env [] _ _ pt = Just (env, pt)
  go i env ([] : goals) active clauses pt = go i env goals active clauses pt
  go i env goals Nothing (clause : clauses) pt = go i env goals (Just clause) clauses pt
  go i env goals (Just clause) clauses pt =
    case resolve env goals namedClause of
      Just (env', goals') | pt' <- connect pt goals' namedClause ->
        go (i + 1) env' goals' Nothing originalClauses pt'
        <|>
        -- Despite resolution succeeding right now, derivation fails somewhere
        -- down the line, try the next clause discarding the additional
        -- unifications added to the environment. In other words, we
        -- _backtrack_ on failure.
        go i env goals Nothing clauses pt
      _ ->
        -- Resolution with the current clause failed, try the next one.
        go i env goals Nothing clauses pt
    where
    namedClause = nameClause i clause
  go _ _ _ _ [] _ = Nothing

run :: [ Clause ] -> Atom -> Maybe Atom
run originalClauses query =
  case derive originalClauses query of
    Just (env, ()) -> Just (substAtom env query)
    Nothing -> Nothing

runWithProvenance :: [ Clause ] -> Atom -> Maybe ProvenanceTree
runWithProvenance originalClauses query =
  case derive originalClauses query of
    Just (env, pt) -> Just (substProvenanceTree env pt)
    Nothing -> Nothing
