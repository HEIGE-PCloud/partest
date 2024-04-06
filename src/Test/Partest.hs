{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.Partest where

import Data.Graph
import Data.List ((\\))
import Data.Map (fromList, Map, (!))
import Data.Maybe (fromJust)
import Data.Tree (flatten)
import Test.QuickCheck

data BNF = BChoices Sym | BSeqs Sym | BTerm String Sym
  deriving (Show, Eq)

-- singleTerm = BTerm "singleTerm"
-- seqTerm1 = BTerm "seqTerm1"
-- seqTerm2 = BTerm "seqTerm2"
-- seqTerm = BSeqs [seqTerm1, seqTerm2]
-- choicesTerm1 = BTerm "choicesTerm1"
-- choicesTerm2 = BTerm "choicesTerm2"
-- choicesTerm = BChoices [choicesTerm1, choicesTerm2]
-- recursiveTerm = BChoices [recursiveTerm1, recursiveTerm2]
-- recursiveTerm1 = BChoices [recursiveTerm, BTerm "recursiveTerm1"]
-- recursiveTerm2 = BChoices [recursiveTerm, BTerm "recursiveTerm2"]

type BNF' = (BNF, Integer, [Integer])


es :: [BNF']
es =
  [ (BTerm "[singleTerm]" (Sym "singleTerm"), 0, [])
  , (BTerm "[seqTerm1]" (Sym "seqTerm1"), 1, [])
  , (BTerm "[seqTerm2]" (Sym "seqTerm2"), 2, [])
  , (BSeqs (Sym "seqTerm"), 3, [1, 2])
  , (BTerm "[choicesTerm1]" (Sym "choicesTerm1"), 4, [])
  , (BTerm "[choicesTerm2]" (Sym "choicesTerm2"), 5, [])
  , (BChoices (Sym "choicesTerm"), 6, [4, 5])
  , (BTerm "[recursiveTerm1]" (Sym "recursiveTerm1'"), 7, [])
  , (BTerm "[recursiveTerm2]" (Sym "recursiveTerm2'"), 8, [])
  , (BChoices (Sym "recursiveTerm"), 9, [10, 11])
  , (BChoices (Sym "recursiveTerm1"), 10, [9, 7])
  , (BChoices (Sym "recursiveTerm2"), 11, [9, 8])
  ]

ms :: Map Integer BNF'
ms =
  fromList
    [ (0, (BTerm "[singleTerm]" (Sym "singleTerm"), 0, []))
    , (1, (BTerm "[seqTerm1]" (Sym "seqTerm1"), 1, []))
    , (2, (BTerm "[seqTerm2]" (Sym "seqTerm2"), 2, []))
    , (3, (BSeqs (Sym "seqTerm"), 3, [1, 2]))
    , (4, (BTerm "[choicesTerm1]" (Sym "choicesTerm1"), 4, []))
    , (5, (BTerm "[choicesTerm2]" (Sym "choicesTerm2"), 5, []))
    , (6, (BChoices (Sym "choicesTerm"), 6, [4, 5]))
    , (7, (BTerm "[recursiveTerm1]" (Sym "recursiveTerm1'"), 7, []))
    , (8, (BTerm "[recursiveTerm2]" (Sym "recursiveTerm2'"), 8, []))
    , (9, (BChoices (Sym "recursiveTerm"), 9, [10, 11]))
    , (10, (BChoices (Sym "recursiveTerm1"), 10, [9, 7]))
    , (11, (BChoices (Sym "recursiveTerm2"), 11, [9, 8]))
    ]

graph :: Graph
nodeFromVertex :: Vertex -> (BNF, Integer, [Integer])
vertexFromKey :: Integer -> Maybe Vertex
(graph, nodeFromVertex, vertexFromKey) = graphFromEdges es


-- This is wrong, use isTerminal' instead
isTerminal :: Integer -> Bool
isTerminal x = any (path graph v) (reachable graph v \\ [v])
  where
    v = fromJust $ vertexFromKey x


r1 :: [Tree Vertex]
r1 = scc graph
r2 :: [SCC BNF]

r3 :: [Vertex]
r2 = stronglyConnComp es

r3 = flatten $ head r1

isSingle :: [a] -> Bool
isSingle [_] = True
isSingle _ = False

ts :: [[Vertex]]
nts :: [[Vertex]]
(ts, nts) = filter' isSingle $ map flatten (scc graph)

terminals :: [Integer]
terminals = map (second . nodeFromVertex . head) ts
isTerminal' :: Integer -> Bool

isTerminal' x = x `elem` terminals

second :: (a, b, c) -> b
nonTerminals :: [Integer]
second (_, x, _) = x

nonTerminals = map (second . nodeFromVertex) (concat nts)

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
choicesTerm = Rule (Sym "choicesTerm") (Choices [Sym "choicesTerm1", Sym "choicesTerm2"]) Defined

recursiveTerm1' :: Rule
recursiveTerm1' = Rule (Sym "recursiveTerm1'") (Term "[recursiveTerm1]") Defined

recursiveTerm2' :: Rule
recursiveTerm2' = Rule (Sym "recursiveTerm2'") (Term "[recursiveTerm2]") Defined

recursiveTerm :: Rule
recursiveTerm = Rule (Sym "recursiveTerm") (Choices [Sym "recursiveTerm1", Sym "recursiveTerm2"])  Defined

recursiveTerm1 :: Rule
recursiveTerm1 = Rule (Sym "recursiveTerm1") (Choices [Sym "recursiveTerm", Sym "recursiveTerm1'"]) Defined

recursiveTerm2 :: Rule
recursiveTerm2 = Rule (Sym "recursiveTerm2") (Choices [Sym "recursiveTerm", Sym "recursiveTerm2'"]) Defined


compile :: [Rule] -> [(BNF, Integer, [Integer])]
compile rs = zip3 bs is es''
  where
    m = assignIds rs
    bs :: [BNF] = map compileExpr rs
    es'' :: [[Integer]] = map (`compileEdge` m) rs
    is :: [Integer] = map (`compileId` m) rs

inverseMap :: [(BNF, Integer, [Integer])] -> Map Integer BNF'
inverseMap = fromList . map (\(b, i, xs) -> (i, (b, i, xs)))

assignIds :: [Rule] -> Map Sym Integer
assignIds rs = fromList $ zip (map symbol rs) [0 ..]

symbol :: Rule -> Sym
symbol (Rule s _ _) = s

compileExpr :: Rule -> BNF
compileExpr (Rule sym (Choices _) _) = BChoices sym
compileExpr (Rule sym (Seqs _) _) = BSeqs sym
compileExpr (Rule sym (Term s) _) = BTerm s sym

compileEdge :: Rule -> Map Sym Integer -> [Integer]
compileEdge (Rule _ expr _) m = map (m !) (flattenExpr expr)

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


gen :: BNF' -> Map Integer BNF' -> Int -> Gen (Tree Res)
gen (BTerm s sym, _, _) _ _ = return $ Node (RTerm s sym) []
gen (BSeqs sym, _, xs) m i = do
  fs <- mapM (\x' -> gen (m ! x') m (i - 1)) xs
  return (Node (RSeq sym) fs)
gen (BChoices sym, _, xs) m depth
  | depth < 1 = do
    let ts' = filter isTerminal' xs
    case ts' of 
      [] -> gen' xs
      _ -> gen' ts'
  | otherwise = gen' xs
  where
    gen' :: [Integer] -> Gen (Tree Res)
    gen' xss = do
      j <- elements xss
      res <- gen (m ! j) m (depth - 1)
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

es' :: [(BNF, Integer, [Integer])]
es' = compile rules

ms' :: Map Integer BNF'
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
baseType = Rule (Sym "baseType") (Choices [Sym "int", Sym "bool", Sym "char", Sym "string"]) Defined

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
ttype = Rule (Sym "ttype") (Choices [Sym "baseType", Sym "arrayType", Sym "pairType"]) Defined

arrayType :: Rule
arrayType = Rule (Sym "arrayType") (Seqs [Sym "ttype", Sym "leftSquareBracket", Sym "rightSquareBracket"]) Defined

pairType :: Rule
pairType = Rule (Sym "pairType") (Seqs [Sym "pair", Sym "leftParam", Sym "pairElemType", Sym "comma", Sym "pairElemType", Sym "rightParam"]) Defined

pairElemType :: Rule
pairElemType = Rule (Sym "pairElemType") (Choices [Sym "baseType", Sym "arrayType", Sym "pair"]) Defined

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

es'' :: [(BNF, Integer, [Integer])]
es'' = compile rules'

ms'' :: Map Integer BNF'
ms'' = inverseMap es''

g' :: Int -> IO ()
g' x = do 
  xs <- sample' $ genAll es'' x
  mapM_ (putStrLn . showTree) xs

genAll :: [BNF'] -> Int -> Gen (Tree Res)
genAll bs i = do
  j <- elements (filter (\(_, key :: Integer, _) -> defined (rules' !! fromInteger key)) bs)
  gen j ms'' i

-- use `g' 10` to sample some examples