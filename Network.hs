module Network where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.List

import           AST

data Network = Network [(Cursor, Node)] [(Cursor, Edge, Cursor)] deriving (Eq, Show)

data ConstValue = ConstNumber Double
                | ConstString String deriving (Eq, Show)

type ValueSource = Maybe ConstValue

data Node = IdentityNode ValueSource
          | InfixNode ValueSource String ValueSource
          | PrefixNode String ValueSource
          | ApplyNode [ValueSource] deriving (Eq, Show)

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

exprToValueSource (Number n) = Just (ConstNumber n)
exprToValueSource (String s) = Just (ConstString s)
exprToValueSource _          = Nothing

merge :: Network -> Network -> Network
merge (Network nodes1 edges1) (Network nodes2 edges2) = Network (nodes1 ++ nodes2) (edges1 ++ edges2)

netFromDefinitions :: [Definition] -> Network
netFromDefinitions = foldl merge (Network [] []) . map netFromDefinition

netFromDefinition :: Definition -> Network
netFromDefinition (Definition name value) = netFromExpr value (R name)

netFromExpr :: Expression -> Cursor -> Network
netFromExpr (Reference r) (R r')   = Network [(R r', IdentityNode Nothing)] [(R r, IdentityArgument, R r')]
netFromExpr (Reference r) (PA c)   = Network [] [(R r, PrefixArgument, c)]
netFromExpr (Reference r) (IL c)   = Network [] [(R r, InfixLeft, c)]
netFromExpr (Reference r) (IR c)   = Network [] [(R r, InfixRight, c)]
netFromExpr (Reference r) (AF c)   = Network [] [(R r, ApplyFunction, c)]
netFromExpr (Reference r) (AA i c) = Network [] [(R r, ApplyArgument i, c)]

netFromExpr (Prefix o a) c
  | isConstantExpression a = Network [(c, PrefixNode o (exprToValueSource a))] [] -- removed in netSquash
  | otherwise = Network [(c, PrefixNode o Nothing)] [(PA c, PrefixArgument, c)] `merge` netFromExpr a (PA c)

netFromExpr (Infix o a b) c 
  | isConstantExpression a &&
    isConstantExpression b = Network [(c, InfixNode (exprToValueSource a) o (exprToValueSource b))] [] -- removed in netSquash
  | isConstantExpression a = Network [(c, InfixNode (exprToValueSource a) o Nothing)] [(IR c, InfixRight, c)]
                             `merge` netFromExpr b (IR c)
  | isConstantExpression a = Network [(c, InfixNode Nothing o (exprToValueSource b))] [(IL c, InfixLeft, c)]
                             `merge` netFromExpr a (IL c)
  | otherwise = Network [(c, InfixNode Nothing o Nothing)]
                        [(IL c, InfixLeft, c), (IR c, InfixRight, c)]
                `merge` netFromExpr a (IL c)
                `merge` netFromExpr b (IR c)

netFromExpr (Application f as) c = Network [(c, ApplyNode vss)]
                                           ([(AF c, ApplyFunction, c)] ++ map (\(n, _) -> (AA n c, ApplyArgument n, c)) as')
                                   `merge` foldl merge (netFromExpr f (AF c)) (map (\(n, a) -> netFromExpr a (AA n c)) as')
  where (as', vss) = foldr grabConstants ([], []) ([0 .. length as - 1] `zip` as)
        grabConstants (n, a) (as', vss)
          | isConstantExpression a = (as', exprToValueSource a : vss)
          | otherwise              = ((n, a) : as', Nothing : vss)

netFromExpr a c | isConstantExpression a = Network [(c, IdentityNode (exprToValueSource a))] []

netSquash :: Network -> Network
netSquash net@(Network nodes _) = if net' == net
                                     then net'
                                     else netSquash net'
  where net' = foldl netSquashFold net nodes

netSquashFold :: Network -> (Cursor, Node) -> Network
netSquashFold (Network nodes edges) node@(c,IdentityNode (Just v)) =
  constantizeCursor c v (Network (delete node nodes) edges)

netSquashFold (Network nodes edges) node@(c,PrefixNode "-" (Just (ConstNumber a))) =
  constantizeCursor c (ConstNumber (-a)) (Network (delete node nodes) edges)

netSquashFold (Network nodes edges) node@(c,InfixNode (Just (ConstString a)) "+" (Just (ConstString b))) =
  constantizeCursor c (ConstString $ a ++ b) (Network (delete node nodes) edges)

netSquashFold (Network nodes edges) node@(c,InfixNode (Just (ConstNumber a)) "+" (Just (ConstNumber b))) =
  constantizeCursor c (ConstNumber $ a + b) (Network (delete node nodes) edges)
netSquashFold (Network nodes edges) node@(c,InfixNode (Just (ConstNumber a)) "-" (Just (ConstNumber b))) =
  constantizeCursor c (ConstNumber $ a - b) (Network (delete node nodes) edges)
netSquashFold (Network nodes edges) node@(c,InfixNode (Just (ConstNumber a)) "*" (Just (ConstNumber b))) =
  constantizeCursor c (ConstNumber $ a * b) (Network (delete node nodes) edges)
netSquashFold (Network nodes edges) node@(c,InfixNode (Just (ConstNumber a)) "/" (Just (ConstNumber b))) =
  constantizeCursor c (ConstNumber $ a / b) (Network (delete node nodes) edges)

netSquashFold net _ = net

constantizeCursor :: Cursor -> ConstValue -> Network -> Network
constantizeCursor c v (Network nodes edges) = Network (map updateNode nodes) (edges \\ oe)
  where oe = outboundEdges c edges
        updateNode (c', n) = case filter (\(_, _, b) -> b == c') oe of
                                  [(_, IdentityArgument, _)] ->
                                    case n of IdentityNode Nothing  -> (c', IdentityNode (Just v))
                                  [(_, PrefixArgument, _)] ->
                                    case n of PrefixNode o Nothing  -> (c', PrefixNode o (Just v))
                                  [(_, InfixLeft, _)] ->
                                    case n of InfixNode Nothing o b -> (c', InfixNode (Just v) o b)
                                  [(_, InfixRight, _)] ->
                                    case n of InfixNode a o Nothing -> (c', InfixNode a o (Just v))
                                  [(_, ApplyArgument i, _)] ->
                                    case n of ApplyNode as
                                                | as !! i == Nothing -> let (a, b) = splitAt i as
                                                                            in (c', ApplyNode (a ++ Just v : tail b))
                                  _ -> (c', n)

outboundEdges :: Cursor -> [(Cursor, Edge, Cursor)] -> [(Cursor, Edge, Cursor)]
outboundEdges c = filter (\(a, _, _) -> a == c)

inboundEdges :: Cursor -> [(Cursor, Edge, Cursor)] -> [(Cursor, Edge, Cursor)]
inboundEdges c = filter (\(_, _, b) -> b == c)

edgesAffectedBy :: Cursor -> [(Cursor, Edge, Cursor)] -> [(Cursor, Edge, Cursor)]
edgesAffectedBy c edges = nub $ oe ++ concatMap (flip edgesAffectedBy edges) oc
  where oe = outboundEdges c edges
        oc = map (\ (_, _, b) -> b) oe

makeDestinationMap :: [(Cursor, Edge, Cursor)] -> Map Cursor [(Edge, Cursor)]
makeDestinationMap edges = foldl (\ m (src, edge, dest) -> Map.alter (\ l -> Just $ (edge, dest) : maybe [] id l) src m) Map.empty edges

makeNodeMap :: [(Cursor, Node)] -> Map Cursor [(Edge, Cursor)] -> [(Cursor, Node, [(Edge, Cursor)])]
makeNodeMap nodes destinationMap = map (\ (cursor, node) -> (cursor, node, Map.findWithDefault [] cursor destinationMap)) nodes
