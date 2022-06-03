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
  connect :: p -> GoalStack -> Clause -> p

instance Provenance () where
  unit _ = ()
  connect _ _ _ = ()

--------------------------------------------------------------------------------
-- Full-fidelity provenance tree
--------------------------------------------------------------------------------

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

instance Provenance ProvenanceTree where
  unit = PLeaf . concat
  connect pt gs = PNode pt (concat gs)

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

addEdge :: G.Node -> G.Node -> GraphBuilderM ()
addEdge src dst = St.modify (\st -> st {_edges = (src, dst, "") : _edges st})

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
  go (PNode pt gs cl) = do
    src1 <- go pt
    src2 <- newNode
    dst <- newNode
    addNode (src2, show cl)
    addNode (dst, show gs)
    addEdge src1 dst
    addEdge src2 dst
    pure dst

visualise :: ProvenanceTree -> FilePath -> IO FilePath
visualise pt = GV.runGraphviz dotGraph GV.Jpeg
  where
  pg = toGraph pt
  dotGraph = GV.graphToDot GV.quickParams pg
