module JavaScript where

import           Data.List
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Text.JSON as JSON

import           AST
import           Network

programToJavaScript :: Program -> String

programToJavaScript (Program interfaceDeclarations definitions) =
  jsNodeSpace interfaceDeclarations destinationMap nodeMap

  where Network nodes edges = netFromDefinitions definitions
        destinationMap      = makeDestinationMap edges
        nodeMap             = makeNodeMap nodes destinationMap

makeDestinationMap :: [(Cursor, Edge, Cursor)] -> Map Cursor [(Edge, Cursor)]

makeDestinationMap edges = foldl (\ m (src, edge, dest) -> Map.alter (\ l -> Just $ (edge, dest) : maybe [] id l) src m) Map.empty edges

makeNodeMap :: [(Cursor, Node)] -> Map Cursor [(Edge, Cursor)] -> [(Cursor, Node, [(Edge, Cursor)])]

makeNodeMap nodes destinationMap = map (\ (cursor, node) -> (cursor, node, Map.findWithDefault [] cursor destinationMap)) nodes

jsNodeSpace :: [InterfaceDeclaration] -> Map Cursor [(Edge, Cursor)] -> [(Cursor, Node, [(Edge, Cursor)])] -> String

jsNodeSpace interfaceDeclarations destinationMap nodeMap =
  "var ns={" ++ intercalate "," ("imports:{},exports:{}" : map nodeDefinition nodeMap) ++ "};" ++ initInterface ++ initConstants

  where nodeDefinition (cursor, node, destinations) =
          cursorToIdentifier cursor ++ ":" ++
          jsNode node (exportCode cursor ++ jsEdges "value" destinations)

        initConstants = concat ["ns." ++ cursorToIdentifier cursor ++ ".update();"
                               | (cursor, node, _) <- nodeMap
                               , isConstantNode node]

        initInterface = flip concatMap interfaceDeclarations $ \ declaration ->
                          case declaration of
                               Import r -> "ns.imports[" ++ JSON.encode r ++ "]=function(value){"
                                        ++ maybe "" (jsEdges "value") (Map.lookup (R r) destinationMap)
                                        ++ "};"

                               Export r -> "ns.exports[" ++ JSON.encode r ++ "]={value:null,update:function(){}};"

        exportCode cursor = case cursor of
                                 R r | Export r `elem` interfaceDeclarations
                                    -> "ns.exports[" ++ JSON.encode r ++ "].value=value;"
                                    ++ "ns.exports[" ++ JSON.encode r ++ "].update();"
                                 _  -> ""

jsNode IdentityNode   updateCode = "{identity:null,update:function(){var value=this.identity;" ++ updateCode ++ "}}"
jsNode (NumberNode n) updateCode = "{update:function(){var value=" ++ JSON.encode n ++ ";" ++ updateCode ++ "}}"
jsNode (StringNode s) updateCode = "{update:function(){var value=" ++ JSON.encode s ++ ";" ++ updateCode ++ "}}"
jsNode (InfixNode o)  updateCode = "{left:null,right:null,update:function(){var value=this.left!==null&&this.right!==null?this.left" ++ o ++ "this.right:null;" ++ updateCode ++ "}}"
jsNode (PrefixNode o) updateCode = "{prefixArg:null,update:function(){var value=this.prefixArg!==null?" ++ o ++ "(this.prefixArg):null;" ++ updateCode ++ "}}"
jsNode ApplyNode      updateCode = "{fn:null,fnArgs:[],update:function(){var value=typeof this.fn==='function'?this.fn.apply(null,this.fnArgs):null;" ++ updateCode ++ "}}"

jsEdges srcExpression = concat . uncurry (++) . unzip . map (jsEdge srcExpression)

jsEdge srcExpression (edge, destCursor) = (edgeSet edge, destName ++ ".update();")
  where destName = "ns." ++ cursorToIdentifier destCursor

        edgeSet  IdentityArgument = destName ++ ".identity="  ++ srcExpression ++ ";"
        edgeSet  InfixLeft        = destName ++ ".left="      ++ srcExpression ++ ";"
        edgeSet  InfixRight       = destName ++ ".right="     ++ srcExpression ++ ";"
        edgeSet  PrefixArgument   = destName ++ ".prefixArg=" ++ srcExpression ++ ";"
        edgeSet  ApplyFunction    = destName ++ ".fn="        ++ srcExpression ++ ";"
        edgeSet (ApplyArgument i) = destName ++ ".fnArgs["    ++ JSON.encode i
                                             ++        "]="   ++ srcExpression ++ ";"

cursorToIdentifier (R    r) = "r" ++ show (length r) ++ "_" ++ r
cursorToIdentifier (PA   c) = cursorToIdentifier c ++ "pa"
cursorToIdentifier (IL   c) = cursorToIdentifier c ++ "il"
cursorToIdentifier (IR   c) = cursorToIdentifier c ++ "ir"
cursorToIdentifier (AF   c) = cursorToIdentifier c ++ "af"
cursorToIdentifier (AA i c) = cursorToIdentifier c ++ "aa" ++ show i
