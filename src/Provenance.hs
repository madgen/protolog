module Provenance where

import AST
import Unification
import Substitution

type GoalStack = [ [ Atom ] ]
type FlatGoalStack = [ Atom ]

data ProvenanceTree =
    PNode ProvenanceTree FlatGoalStack Clause
  | PLeaf FlatGoalStack
  deriving (Show, Eq)

substProvenanceTree :: Env -> ProvenanceTree -> ProvenanceTree
substProvenanceTree env (PLeaf gs) = PLeaf $ substFlatGoalStack env gs
substProvenanceTree env (PNode pt gs cl) =
  PNode
    (substProvenanceTree env pt)
    (substFlatGoalStack env gs)
    (substClause env cl)

substFlatGoalStack :: Env -> FlatGoalStack -> FlatGoalStack
substFlatGoalStack env = map (substAtom env)

class Provenance p where
  unit :: GoalStack -> p
  connect :: p -> GoalStack -> Clause -> p

instance Provenance () where
  unit _ = ()
  connect _ _ _ = ()

instance Provenance ProvenanceTree where
  unit = PLeaf . concat
  connect pt gs = PNode pt (concat gs)
