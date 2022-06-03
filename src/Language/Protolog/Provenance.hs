module Language.Protolog.Provenance where

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.PatriciaTree as G
import qualified Data.GraphViz as GV

import qualified Control.Monad.Trans.State as St

import Language.Protolog.AST
import Language.Protolog.Unification
import Language.Protolog.Substitution

class Provenance p where
  unit :: GoalStack -> p
  connect :: p -> Env -> GoalStack -> Clause -> p

instance Provenance () where
  unit _ = ()
  connect _ _ _ _ = ()

--------------------------------------------------------------------------------
-- Full-fidelity provenance tree
--------------------------------------------------------------------------------

type GoalStack = [ [ Literal ] ]
type FlatGoalStack = [ Literal ]

data ProvenanceTree =
    PNode ProvenanceTree Env FlatGoalStack Clause
  | PLeaf FlatGoalStack
  deriving (Show, Eq)

substProvenanceTree :: ProvenanceTree -> ProvenanceTree
substProvenanceTree (PLeaf gs) = PLeaf gs
substProvenanceTree (PNode pt env gs cl) =
  PNode
    (substProvenanceTree pt)
    env
    (substFlatGoalStack env gs)
    cl

substFlatGoalStack :: Env -> FlatGoalStack -> FlatGoalStack
substFlatGoalStack env = map (substLiteral env)

instance Provenance ProvenanceTree where
  unit = PLeaf . concat
  connect pt env gs = PNode pt env (concat gs)

--------------------------------------------------------------------------------
-- Convert provenance tree to a graph for visualisation
--------------------------------------------------------------------------------

type ProvenanceGraph = G.Gr String String

type Node = G.LNode String
type Edge = G.LEdge String

data GraphBuilderSt = GraphBuilderSt
  { _counter :: Int
  , _nodes :: [ Node ]
  , _edges :: [ Edge ]
  }

type GraphBuilderM a = St.State GraphBuilderSt a

newNode :: GraphBuilderM G.Node
newNode = do
  n <- _counter <$> St.get
  St.modify (\st -> st {_counter = n + 1})
  pure n

addNode :: Node -> GraphBuilderM ()
addNode node = St.modify (\st -> st {_nodes = node : _nodes st})

addEdge :: G.Node -> G.Node -> [ Substitution ] -> GraphBuilderM ()
addEdge src dst substs =
  St.modify (\st -> st {_edges = (src, dst, label) : _edges st})
  where
  label = concatMap show substs

mkGraph :: GraphBuilderM a -> ProvenanceGraph
mkGraph action = G.mkGraph (_nodes st) (_edges st)
  where
  initSt = GraphBuilderSt {_counter = 0, _nodes = [], _edges = []}
  st = St.execState action initSt

toGraph :: ProvenanceTree -> ProvenanceGraph
toGraph = mkGraph . go
  where
  go :: ProvenanceTree -> GraphBuilderM G.Node
  go (PLeaf gs) = do
    node <- newNode
    addNode (node, show gs)
    pure node
  go (PNode pt env gs cl@(head :- _)) = do
    src1 <- go pt
    src2 <- newNode
    dst <- newNode
    addNode (src2, show cl)
    addNode (dst, show gs)
    let substs1 = edgeSubsts env $ headGoal pt
    addEdge src1 dst substs1
    let substs2 = edgeSubsts env head
    addEdge src2 dst substs2
    pure dst

  edgeSubsts env = filter (not . isTrivialSubst) . fmap (mkSubst env) . vars

headGoal :: ProvenanceTree -> Literal
headGoal (PNode _ _ [] _) = error "Called it on the root node"
headGoal (PNode _ _ (l : _) _) = l
headGoal (PLeaf []) = error "Impossible: Called it on the root node which is also a leaf!"
headGoal (PLeaf (l : _)) = l

visualise :: ProvenanceTree -> FilePath -> IO FilePath
visualise pt = GV.runGraphviz dotGraph GV.Jpeg
  where
  pg = toGraph pt
  dotGraph = GV.graphToDot GV.quickParams pg
