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

derive :: [ Clause ] -> Atom -> Maybe (Atom, ProvenanceTree)
derive originalClauses query =
  case go 0 P.empty queryGoalStack Nothing originalClauses queryProvenance of
    Just (env, pt) -> Just (substAtom env query, substProvenanceTree env pt)
    Nothing -> Nothing
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

substClause :: Env -> Clause -> Clause
substClause env (head :- body) = substAtom env head :- (substAtom env <$> body)

substGoalStack :: Env -> GoalStack -> GoalStack
substGoalStack env = fmap (fmap (substAtom env))
