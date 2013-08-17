module Network where
import AST
import Data.Maybe
import Data.List

data Network = Network [(Cursor, Node)] [(Cursor, Edge, Cursor)] deriving (Show)

data Node = IdentityNode
          | NumberNode Double
          | StringNode String
          | InfixNode String
          | PrefixNode String
          | ApplyNode deriving (Eq, Show)
data Edge = IdentityArgument
          | InfixLeft
          | InfixRight
          | PrefixArgument
          | ApplyFunction
          | ApplyArgument Int deriving (Eq, Show)
data Cursor = R String
            | PA Cursor
            | IL Cursor
            | IR Cursor
            | AF Cursor
            | AA Int Cursor deriving (Ord, Eq, Show)
              -- Ord instance is just for Map

isConstantNode (NumberNode _) = True
isConstantNode (StringNode _) = True
isConstantNode _              = False

merge :: Network -> Network -> Network
merge (Network nodes1 edges1) (Network nodes2 edges2) = Network (nodes1 ++ nodes2) (edges1 ++ edges2)

netFromDefinitions :: [Definition] -> Network
netFromDefinitions = foldl merge (Network [] []) . map netFromDefinition

netFromDefinition :: Definition -> Network
netFromDefinition (Definition name value) = netFromExpr value (R name)

netFromExpr :: Expression -> Cursor -> Network
netFromExpr (Number n) c = Network [(c, NumberNode n)] []
netFromExpr (String s) c = Network [(c, StringNode s)] []
netFromExpr (Reference r) c = Network [(c, IdentityNode)] [(R r, IdentityArgument, c)]
netFromExpr (Prefix o a) c = Network [(c, PrefixNode o)] [(PA c, PrefixArgument, c)] `merge` netFromExpr a (PA c)
netFromExpr (Infix o a b) c = Network [(c, InfixNode o)]
                                      [(IL c, InfixLeft, c), (IR c, InfixRight, c)]
                              `merge` netFromExpr a (IL c)
                              `merge` netFromExpr b (IR c)
netFromExpr (Application f as) c = Network [(c, ApplyNode)]
                                           ([(AF c, ApplyFunction, c)] ++ map (\n -> (AA n c, ApplyArgument n, c)) [0 .. length as - 1])
                                   `merge` foldl merge (netFromExpr f (AF c)) (zipWith (\a n -> netFromExpr a (AA n c)) as [0 .. length as - 1])

outboundEdges :: Cursor -> [(Cursor, Edge, Cursor)] -> [(Cursor, Edge, Cursor)]
outboundEdges c = filter (\(a, _, _) -> a == c)

inboundEdges :: Cursor -> [(Cursor, Edge, Cursor)] -> [(Cursor, Edge, Cursor)]
inboundEdges c = filter (\(_, _, b) -> b == c)

edgesAffectedBy :: Cursor -> [(Cursor, Edge, Cursor)] -> [(Cursor, Edge, Cursor)]
edgesAffectedBy c edges = nub $ oe ++ concatMap (flip edgesAffectedBy edges) oc
  where oe = outboundEdges c edges
        oc = map (\ (_, _, b) -> b) oe
