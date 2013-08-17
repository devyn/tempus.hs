module JavaScript where

import           Data.List
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Text.JSON as JSON

import           AST
import           Network

programToJavaScript :: Program -> String

programToJavaScript (Program interfaceDeclarations definitions) =
  jsNodeSpace interfaceDeclarations (netSquash $ netFromDefinitions definitions)

jsNodeSpace :: [InterfaceDeclaration] -> Network -> String

jsNodeSpace interfaceDeclarations network@(Network nodes edges) =
  "var ns={" ++ intercalate "," ( ("imports:{" ++ importStubs ++ "},exports:{" ++ exportStubs ++ "}")
                                : map nodeDefinition nodeMap
                                ) ++ "};"

  where destinationMap = makeDestinationMap edges
        nodeMap        = makeNodeMap nodes destinationMap
  
        nodeDefinition (cursor, node, destinations) =
          cursorToIdentifier cursor ++ ":" ++
          jsNode node (exportCode cursor ++ jsEdges "value" destinations)

        importStubs = intercalate "," $ map importStub [r | Import r <- interfaceDeclarations]
        exportStubs = intercalate "," $ map exportStub [r | Export r <- interfaceDeclarations]

        importStub r = JSON.encode r ++ ":function(value){"
                                     ++ waitValuesFromImport network r
                                     ++ maybe "" (jsEdges "value") (Map.lookup (R r) destinationMap)
                                     ++ "}"

        exportStub r = JSON.encode r ++ ":{value:null,update:function(){}}"

        exportCode cursor = case cursor of
                                 R r | Export r `elem` interfaceDeclarations
                                    -> "ns.exports[" ++ JSON.encode r ++ "].value=value;"
                                    ++ "ns.exports[" ++ JSON.encode r ++ "].update();"
                                 _  -> ""

jsNode (IdentityNode a)  updateCode = "{identity:" ++ jsConst a ++ ",update:function(){var value=this.identity;" ++ updateCode ++ "}}"
jsNode (PrefixNode o a)  updateCode = "{prefixArg:" ++ jsConst a ++ ",update:function(){var value=this.prefixArg!==null?" ++ o ++ "(this.prefixArg):null;" ++ updateCode ++ "}}"
jsNode (InfixNode a o b) updateCode = "{wait:0,left:" ++ jsConst a ++",right:" ++ jsConst b ++ ",update:function(){if(this.wait<1){var value=this.left!==null&&this.right!==null?this.left" ++ o ++ "this.right:null;" ++ updateCode ++ "}else this.wait--;}}"
jsNode (ApplyNode as)    updateCode = "{wait:0,fn:null,fnArgs:" ++ jsConsts as ++ ",update:function(){if(this.wait<1){var value=typeof this.fn==='function'?this.fn.apply(null,this.fnArgs):null;" ++ updateCode ++ "}else this.wait--;}}"

jsConst (Just (ConstNumber n)) = JSON.encode n
jsConst (Just (ConstString s)) = JSON.encode s
jsConst Nothing                = "null"

jsConsts = JSON.encode . JSON.JSArray . map (\ x -> case x of
                                                         Just (ConstNumber n) -> JSON.showJSON n
                                                         Just (ConstString s) -> JSON.showJSON s
                                                         Nothing              -> JSON.JSNull)

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

waitValuesFromImport (Network nodes edges) r = concatMap waitStatement . filter ((> 0) . snd) $ map incomingCount nodes
  where edges' = edgesAffectedBy (R r) edges

        incomingCount (c, InfixNode _ _ _) = (c, length (inboundEdges c edges') - 1)
        incomingCount (c, ApplyNode _)     = (c, length (inboundEdges c edges') - 1)
        incomingCount (c, _)               = (c, 0) -- even though it could be 1, we don't care

        waitStatement (c, n) = "ns." ++ cursorToIdentifier c ++ ".wait=" ++ JSON.encode n ++ ";"

cursorToIdentifier (R    r) = "r" ++ show (length r) ++ "_" ++ r
cursorToIdentifier (PA   c) = cursorToIdentifier c ++ "pa"
cursorToIdentifier (IL   c) = cursorToIdentifier c ++ "il"
cursorToIdentifier (IR   c) = cursorToIdentifier c ++ "ir"
cursorToIdentifier (AF   c) = cursorToIdentifier c ++ "af"
cursorToIdentifier (AA i c) = cursorToIdentifier c ++ "aa" ++ show i

debug c v = "console.log(" ++ JSON.encode (show c) ++ "+' = '+value);"
