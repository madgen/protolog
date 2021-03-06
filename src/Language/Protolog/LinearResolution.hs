{-# LANGUAGE ScopedTypeVariables #-}

module Language.Protolog.LinearResolution
  ( run
  , runWithProvenance
  ) where

import qualified Data.Partition as P
import Data.Maybe (fromMaybe)

import Control.Applicative ((<|>))
import Control.Monad.Trans.Reader as R

import Language.Protolog.AST
import qualified Language.Protolog.Unification as Unify
import qualified Language.Protolog.Naming as Naming
import qualified Language.Protolog.Substitution as Subst
import qualified Language.Protolog.Provenance as Prov

resolve :: Unify.Env -> Prov.GoalStack -> Clause -> Maybe (Unify.Env, Prov.GoalStack)
resolve _ [] _ = error "Why are you resolving?"
resolve _ ([] : _) _ = error "Why are you resolving?"
resolve _ ((Literal Negative _: _) : _) _ = error "Can't resolve a negative goal"
resolve env ((Literal Positive p : ps) : pss) (q :- qs)
  | Just env <- Unify.atom env p q = Just (env, qs : ps : pss)
  | otherwise = Nothing

type ContextM p a = Reader (Int, Unify.Env, p) a

derive :: forall p. Prov.Provenance p => [ Clause ] -> Literal -> Maybe (Unify.Env, p)
derive originalClauses query =
  runReader (go queryGoalStack originalClauses) (0, P.empty, queryProvenance)
  where
  queryGoalStack = [ [ query ] ]
  queryProvenance = Prov.query queryGoalStack

  go :: Prov.GoalStack -> [ Clause ] -> ContextM p (Maybe (Unify.Env, p))
  go [] _ = do
    (_, env, pt) <- R.ask
    pure $ Just (env, pt)
  go ([] : goals) clauses = go goals clauses
  go ((Literal Negative goal : goals) : goalss) clauses = do
    res <- go [ [ Literal Positive goal ] ] clauses
    case res of
      Just _ -> pure Nothing
      Nothing -> local (\(i, env, pt) -> (i, env, Prov.negative pt env (goals : goalss))) $
        go (goals : goalss) clauses
  go goals (clause : clauses) = do
    (i, env, pt) <- R.ask
    let namedClause = Naming.clause i clause
    case resolve env goals namedClause of
      Just (env', goals') | pt' <- Prov.positive pt env' goals' namedClause -> do
        onSuccess <- local (const (i + 1, env', pt')) (go goals' originalClauses)
        onFailure <- go goals clauses
        -- Despite resolution succeeding right now, derivation fails somewhere
        -- down the line, try the next clause discarding the additional
        -- unifications added to the environment. In other words, we
        -- _backtrack_ on failure.
        pure $ onSuccess <|> onFailure
      _ ->
        -- Resolution with the current clause failed, try the next one.
        go goals clauses
  go _ [] = pure Nothing

run :: [ Clause ] -> Literal -> Maybe Literal
run originalClauses query =
  case derive originalClauses query of
    Just (env, ()) -> Just (Subst.literal env query)
    Nothing -> Nothing

runWithProvenance :: [ Clause ] -> Literal -> Maybe Prov.ProvenanceTree
runWithProvenance originalClauses query =
  case derive originalClauses query of
    Just (env, pt) -> Just (Prov.substProvenanceTree pt)
    Nothing -> Nothing
