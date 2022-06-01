module LinearResolution where

import qualified Data.Partition as P

import AST
import Unification
import Naming
import Substitution

type GoalStack = [ [ Atom ] ]

resolve :: Env -> GoalStack -> Clause -> Maybe (Env, GoalStack)
resolve _ [] _ = error "Why are you resolving?"
resolve _ ([] : _) _ = error "Why are you resolving?"
resolve env ((p : ps) : pss) (q :- qs)
  | Just env <- unify env p q = Just (env, qs : ps : pss)
  | otherwise = Nothing

derive :: [ Clause ] -> Atom -> Maybe (Env, ProvenanceTree)
derive originalClauses query =
  go 0 P.empty queryGoalStack Nothing originalClauses queryProvenance
  where
  queryGoalStack = [ [ query ] ]
  queryProvenance = PLeaf queryGoalStack

  go :: Int
     -> Env
     -> GoalStack
     -> Maybe Clause
     -> [ Clause ]
     -> ProvenanceTree
     -> Maybe (Env, ProvenanceTree)
  go _ env [] _ _ pt = Just (env, pt)
  go i env ([] : goals) active clauses pt = go i env goals active clauses pt
  go i env goals Nothing (clause : clauses) pt = go i env goals (Just clause) clauses pt
  go i env goals (Just clause) clauses pt =
    case resolve env goals namedClause of
      Just (env', goals')
        | pt' <- PNode pt goals' namedClause
        , res@Just{} <- go (i + 1) env' goals' Nothing originalClauses pt' ->
          res
      _ -> go i env goals Nothing clauses pt
    where
    namedClause = nameClause i clause
  go _ _ _ _ [] _ = Nothing

run :: [ Clause ] -> Atom -> Maybe Atom
run originalClauses query =
  case derive originalClauses query of
    Just (env, _pt) -> Just (substAtom env query)
    Nothing -> Nothing

runWithProvenance :: [ Clause ] -> Atom -> Maybe ProvenanceTree
runWithProvenance originalClauses query =
  case derive originalClauses query of
    Just (env, pt) -> Just (substProvenanceTree env pt)
    Nothing -> Nothing

data ProvenanceTree =
    PNode ProvenanceTree GoalStack Clause
  | PLeaf GoalStack
  deriving (Show, Eq)

substProvenanceTree :: Env -> ProvenanceTree -> ProvenanceTree
substProvenanceTree env (PLeaf gs) = PLeaf $ substGoalStack env gs
substProvenanceTree env (PNode pt gs cl) =
  PNode
    (substProvenanceTree env pt)
    (substGoalStack env gs)
    (substClause env cl)

substGoalStack :: Env -> GoalStack -> GoalStack
substGoalStack env = fmap (fmap (substAtom env))
