{-# LANGUAGE ScopedTypeVariables #-}

module Test.Partest where

import Control.Monad.State
import Data.Graph
import Data.List.Extra (concatUnzip)
import Data.List.NonEmpty (NonEmpty ((:|)), singleton, toList)
import Data.Map (Map, fromList, (!))
import Data.Tree (flatten)
import Test.Partest.Internal.Parser
  ( PChoices (..)
  , PRules (..)
  , PSeqs (..)
  , PTerm (..)
  , pRules
  )
import Test.QuickCheck

-- GRule is a BNF rule inside the graph
data GRule = GChoices Sym | GSeqs Sym | GTerm String Sym
  deriving (Show, Eq)

type GRuleNode = (GRule, Integer, [Integer])

-- graph :: Graph
-- nodeFromVertex :: Vertex -> (GRule, Integer, [Integer])
-- vertexFromKey :: Integer -> Maybe Vertex
-- (graph, nodeFromVertex, vertexFromKey) = graphFromEdges es

isSingle :: [a] -> Bool
isSingle [_] = True
isSingle _ = False

-- ts :: [[Vertex]]
-- nts :: [[Vertex]]
-- (ts, nts) = filter' isSingle $ map flatten (scc graph)

-- terminals :: [Integer]
-- terminals = map (second . nodeFromVertex . head) ts

-- isTerminal' :: Integer -> Bool
-- isTerminal' x = x `elem` terminals

second :: (a, b, c) -> b
second (_, x, _) = x

-- nonTerminals :: [Integer]
-- nonTerminals = map (second . nodeFromVertex) (concat nts)

filter' :: (a -> Bool) -> [a] -> ([a], [a])
filter' _ [] = ([], [])
filter' f (x : xs)
  | f x = (x : ys, zs)
  | otherwise = (ys, x : zs)
  where
    (ys, zs) = filter' f xs

{-
  <singleTerm> ::= "singleTerm"
  <seqTerm1> ::= "seqTerm1"
  <seqTerm2> ::= "seqTerm2"
  <seqTerm> ::= <seqTerm1> <seqTerm2>
  <choicesTerm1> ::= "choicesTerm1"
  <choicesTerm2> ::= "choicesTerm2"
  <choicesTerm> ::= <choicesTerm1> | <choicesTerm2>
  <recursiveTerm1> ::= <recursiveTerm> | "recursiveTerm1"
  <recursiveTerm2> ::= <recursiveTerm> | "recursiveTerm2"
  <recursiveTerm> ::= <recursiveTerm1> | <recursiveTerm2>
-}

newtype Sym = Sym String
  deriving (Show, Ord, Eq)

data Expr = Choices [Sym] | Seqs [Sym] | Term String
  deriving (Show, Ord, Eq)

data Rule = Rule Sym Expr RuleType
  deriving (Show, Ord, Eq)

-- a rule is either defined by user or extracted from the grammar
data RuleType = Defined | Extracted
  deriving (Show, Ord, Eq)

singleTerm :: Rule
singleTerm = Rule (Sym "singleTerm") (Term "[singleTerm]") Defined

seqTerm1 :: Rule
seqTerm1 = Rule (Sym "seqTerm1") (Term "[seqTerm1]") Defined

seqTerm2 :: Rule
seqTerm :: Rule
seqTerm2 = Rule (Sym "seqTerm2") (Term "[seqTerm2]") Defined

seqTerm = Rule (Sym "seqTerm") (Seqs [Sym "seqTerm1", Sym "seqTerm2"]) Defined

choicesTerm1 :: Rule
choicesTerm1 = Rule (Sym "choicesTerm1") (Term "[choicesTerm1]") Defined

choicesTerm2 :: Rule
choicesTerm2 = Rule (Sym "choicesTerm2") (Term "[choicesTerm2]") Defined

choicesTerm :: Rule
choicesTerm =
  Rule
    (Sym "choicesTerm")
    (Choices [Sym "choicesTerm1", Sym "choicesTerm2"])
    Defined

recursiveTerm1' :: Rule
recursiveTerm1' = Rule (Sym "recursiveTerm1'") (Term "[recursiveTerm1]") Defined

recursiveTerm2' :: Rule
recursiveTerm2' = Rule (Sym "recursiveTerm2'") (Term "[recursiveTerm2]") Defined

recursiveTerm :: Rule
recursiveTerm =
  Rule
    (Sym "recursiveTerm")
    (Choices [Sym "recursiveTerm1", Sym "recursiveTerm2"])
    Defined

recursiveTerm1 :: Rule
recursiveTerm1 =
  Rule
    (Sym "recursiveTerm1")
    (Choices [Sym "recursiveTerm", Sym "recursiveTerm1'"])
    Defined

recursiveTerm2 :: Rule
recursiveTerm2 =
  Rule
    (Sym "recursiveTerm2")
    (Choices [Sym "recursiveTerm", Sym "recursiveTerm2'"])
    Defined

compileNodes :: [Rule] -> [(GRule, Integer, [Integer])]
compileNodes rs = zip3 bs is es'''
  where
    m = assignIds rs
    bs :: [GRule] = map compileExpr rs
    es''' :: [[Integer]] = map (`compileEdge` m) rs
    is :: [Integer] = map (`compileId` m) rs

inverseMap :: [(GRule, Integer, [Integer])] -> Map Integer GRuleNode
inverseMap = fromList . map (\(b, i, xs) -> (i, (b, i, xs)))

assignIds :: [Rule] -> Map Sym Integer
assignIds rs = fromList $ zip (map symbol rs) [0 ..]

symbol :: Rule -> Sym
symbol (Rule s _ _) = s

compileExpr :: Rule -> GRule
compileExpr (Rule sym (Choices _) _) = GChoices sym
compileExpr (Rule sym (Seqs _) _) = GSeqs sym
compileExpr (Rule sym (Term s) _) = GTerm s sym

compileEdge :: Rule -> Map Sym Integer -> [Integer]
compileEdge (Rule _ expr _) m = map (m !) (flattenExpr expr)

compileGraph
  :: [(GRule, Integer, [Integer])]
  -> (Graph, Vertex -> (GRule, Integer, [Integer]), Integer -> Maybe Vertex)
compileGraph = graphFromEdges

flattenExpr :: Expr -> [Sym]
flattenExpr (Seqs ss) = ss
flattenExpr (Choices cs) = cs
flattenExpr (Term _) = []

compileId :: Rule -> Map Sym Integer -> Integer
compileId (Rule s _ _) m = m ! s

data Res = RTerm String Sym | RSeq Sym | RChoices Sym
  deriving (Show)

showTree :: Tree Res -> String
showTree (Node (RTerm s _) _) = s
showTree (Node (RSeq _) ts') = concatMap showTree ts'
showTree (Node (RChoices _) cs) = concatMap showTree cs

gen
  :: GRuleNode -> (Integer -> Bool) -> Map Integer GRuleNode -> Int -> Gen (Tree Res)
gen (GTerm s sym, _, _) _ _ _ = return $ Node (RTerm s sym) []
gen (GSeqs sym, _, xs) isTerminal m i = do
  fs <- mapM (\x' -> gen (m ! x') isTerminal m i) xs
  return (Node (RSeq sym) fs)
gen (GChoices sym, _, xs) isTerminal m depth
  | depth < 1 = do
      let ts' = filter isTerminal xs
      case ts' of
        [] -> gen' xs
        _ -> gen' ts'
  | otherwise = gen' xs
  where
    gen' :: [Integer] -> Gen (Tree Res)
    gen' xss = do
      j <- elements xss
      res <- gen (m ! j) isTerminal m (depth - 1)
      return $ Node (RChoices sym) [res]

rules :: [Rule]
rules =
  [ singleTerm
  , seqTerm1
  , seqTerm2
  , seqTerm
  , choicesTerm1
  , choicesTerm2
  , choicesTerm
  , recursiveTerm1'
  , recursiveTerm2'
  , recursiveTerm
  , recursiveTerm1
  , recursiveTerm2
  ]

es' :: [(GRule, Integer, [Integer])]
es' = compileNodes rules

ms' :: Map Integer GRuleNode
ms' = inverseMap es'

int :: Rule
int = Rule (Sym "int") (Term "int") Extracted

bool :: Rule
bool = Rule (Sym "bool") (Term "bool") Extracted

char :: Rule
char = Rule (Sym "char") (Term "char") Extracted

string :: Rule
string = Rule (Sym "string") (Term "string") Extracted

baseType :: Rule
baseType =
  Rule
    (Sym "baseType")
    (Choices [Sym "int", Sym "bool", Sym "char", Sym "string"])
    Defined

pair :: Rule
pair = Rule (Sym "pair") (Term "pair") Extracted

leftParam :: Rule
leftParam = Rule (Sym "leftParam") (Term "(") Extracted

comma :: Rule
comma = Rule (Sym "comma") (Term ",") Extracted

rightParam :: Rule
rightParam = Rule (Sym "rightParam") (Term ")") Extracted

leftSquareBracket :: Rule
leftSquareBracket = Rule (Sym "leftSquareBracket") (Term "[") Extracted

rightSquareBracket :: Rule
rightSquareBracket = Rule (Sym "rightSquareBracket") (Term "]") Extracted

ttype :: Rule
ttype =
  Rule
    (Sym "ttype")
    (Choices [Sym "baseType", Sym "arrayType", Sym "pairType"])
    Defined

arrayType :: Rule
arrayType =
  Rule
    (Sym "arrayType")
    (Seqs [Sym "ttype", Sym "leftSquareBracket", Sym "rightSquareBracket"])
    Defined

pairType :: Rule
pairType =
  Rule
    (Sym "pairType")
    ( Seqs
        [ Sym "pair"
        , Sym "leftParam"
        , Sym "pairElemType"
        , Sym "comma"
        , Sym "pairElemType"
        , Sym "rightParam"
        ]
    )
    Defined

pairElemType :: Rule
pairElemType =
  Rule
    (Sym "pairElemType")
    (Choices [Sym "baseType", Sym "arrayType", Sym "pair"])
    Defined

rules' :: [Rule]
rules' =
  [ int
  , bool
  , char
  , string
  , baseType
  , pair
  , leftParam
  , comma
  , rightParam
  , leftSquareBracket
  , rightSquareBracket
  , ttype
  , arrayType
  , pairType
  , pairElemType
  ]

defined :: Rule -> Bool
defined (Rule _ _ Defined) = True
defined _ = False

es'' :: [(GRule, Integer, [Integer])]
es'' = compileNodes r2 -- rules'

ms'' :: Map Integer GRuleNode
ms'' = inverseMap es''

g' :: Int -> IO ()
g' x = do
  xs <- sample' $ genAll es'' x
  mapM_ (putStrLn . showTree) xs

genAll :: [GRuleNode] -> Int -> Gen (Tree Res)
genAll bs i = do
  -- build the graph
  let (graph, nodeFromVertex, _vertexFromKey) = graphFromEdges bs
  -- filter out all terminal nodes and non-terminal nodes
  let (tvs, _ntvs) = filter' isSingle $ map flatten (scc graph)
  -- map vertex to keys
  let terminals = map (second . nodeFromVertex . head) tvs
  let isTerminal x = x `elem` terminals
  j <-
    elements
      (filter (\(_, key :: Integer, _) -> defined (r2 !! fromInteger key)) bs)
  gen j isTerminal ms'' i

-- use `g' 10` to sample some examples
{-
data PTerm = PStr String | PSym String

newtype PSeqs = PSeqs (NonEmpty PTerm)

newtype PChoices = PChoices (NonEmpty PSeqs)

newtype PRule = PRule (NonEmpty PChoices)
-}

getSym :: State Integer Sym
getSym = do
  lab <- get
  put (lab + 1)
  return $ Sym ("extractedRule-" ++ show lab)

type Output = ([Rule], [Sym])

compilePTerm :: Maybe Sym -> PTerm -> State Integer Output
compilePTerm sy (PStr s) = do
  sym <- getSym' sy
  return ([Rule sym (Term s) Extracted], [sym])
compilePTerm _ (PSym sym) = return ([], [Sym sym])

getSym' :: Maybe Sym -> State Integer Sym
getSym' (Just s) = return s
getSym' Nothing = getSym

compilePTerms :: Maybe Sym -> NonEmpty PTerm -> State Integer Output
compilePTerms s (pt :| []) = compilePTerm s pt
compilePTerms s ts = do
  rootSym <- getSym' s
  ts' <- mapM (compilePTerm Nothing) (toList ts)
  let (rs, ss) = concatUnzip ts'
  let rootRule = Rule rootSym (Seqs ss) Extracted
  return (rootRule : rs, rootSym : ss)

compilePRules :: PRules -> State Integer Output
compilePRules (PRules rs) = do
  rs' <- mapM compilePChoices rs
  return $ concatUnzip (toList rs')

compilePChoices :: PChoices -> State Integer Output
compilePChoices (PChoices s (ss :| [])) = do
  compilePSeqs (Just (Sym s)) ss
compilePChoices (PChoices sym cs) = do
  cc' <- mapM (compilePSeqs Nothing) (toList cs)
  let (rs, ss) = concatUnzip cc'
  let newSym = Sym sym
  let rootRule = Rule newSym (Choices ss) Defined
  return (rootRule : rs, newSym : ss)

compilePSeqs :: Maybe Sym -> PSeqs -> State Integer Output
compilePSeqs s (PSeqs ts) = compilePTerms s ts

r1 :: State Integer Output
r1 = compilePRules pRules

r2 :: [Rule]
r3 :: Integer
((r2, _), r3) = runState r1 0

r4 :: [Rule]
r4 =
  [ Rule
      (Sym "type")
      (Choices [Sym "baseType", Sym "arrayType", Sym "pairType"])
      Defined
  , Rule
      (Sym "baseType")
      ( Choices
          [ Sym "extractedRule-0"
          , Sym "extractedRule-1"
          , Sym "extractedRule-2"
          , Sym "extractedRule-3"
          ]
      )
      Defined
  , Rule (Sym "extractedRule-0") (Term "int") Extracted
  , Rule (Sym "extractedRule-1") (Term "bool") Extracted
  , Rule (Sym "extractedRule-2") (Term "char") Extracted
  , Rule (Sym "extractedRule-3") (Term "string") Extracted
  , Rule
      (Sym "arrayType")
      (Seqs [Sym "type", Sym "extractedRule-5", Sym "extractedRule-6"])
      Defined
  , Rule (Sym "extractedRule-5") (Term "[") Extracted
  , Rule (Sym "extractedRule-6") (Term "]") Extracted
  , Rule
      (Sym "pairType")
      ( Seqs
          [ Sym "extractedRule-8"
          , Sym "extractedRule-9"
          , Sym "pairElemType"
          , Sym "extractedRule-10"
          , Sym "pairElemType"
          , Sym "extractedRule-11"
          ]
      )
      Defined
  , Rule (Sym "extractedRule-8") (Term "pair") Extracted
  , Rule (Sym "extractedRule-9") (Term "(") Extracted
  , Rule (Sym "extractedRule-10") (Term ",") Extracted
  , Rule (Sym "extractedRule-11") (Term ")") Extracted
  , Rule
      (Sym "pairElemType")
      (Choices [Sym "baseType", Sym "arrayType", Sym "extractedRule-12"])
      Defined
  , Rule (Sym "extractedRule-12") (Term "pair") Extracted
  ]

r5 :: [Rule]
r5 =
  [ Rule
      (Sym "type")
      (Choices [Sym "baseType", Sym "arrayType", Sym "pairType"])
      Defined
  , Rule
      (Sym "baseType")
      ( Choices
          [ Sym "extractedRule-0"
          , Sym "extractedRule-1"
          , Sym "extractedRule-2"
          , Sym "extractedRule-3"
          ]
      )
      Defined
  , Rule (Sym "extractedRule-0") (Term "int") Extracted
  , Rule (Sym "extractedRule-1") (Term "bool") Extracted
  , Rule (Sym "extractedRule-2") (Term "char") Extracted
  , Rule (Sym "extractedRule-3") (Term "string") Extracted
  , Rule
      (Sym "arrayType")
      (Seqs [Sym "type", Sym "extractedRule-4", Sym "extractedRule-5"])
      Extracted
  , Rule (Sym "extractedRule-4") (Term "[") Extracted
  , Rule (Sym "extractedRule-5") (Term "]") Extracted
  , Rule
      (Sym "pairType")
      ( Seqs
          [ Sym "extractedRule-6"
          , Sym "extractedRule-7"
          , Sym "pairElemType"
          , Sym "extractedRule-8"
          , Sym "pairElemType"
          , Sym "extractedRule-9"
          ]
      )
      Extracted
  , Rule (Sym "extractedRule-6") (Term "pair") Extracted
  , Rule (Sym "extractedRule-7") (Term "(") Extracted
  , Rule (Sym "extractedRule-8") (Term ",") Extracted
  , Rule (Sym "extractedRule-9") (Term ")") Extracted
  , Rule
      (Sym "pairElemType")
      (Choices [Sym "baseType", Sym "arrayType", Sym "extractedRule-10"])
      Defined
  , Rule (Sym "extractedRule-10") (Term "pair") Extracted
  ]