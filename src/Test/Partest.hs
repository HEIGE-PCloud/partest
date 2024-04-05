{-# LANGUAGE ScopedTypeVariables #-}

module Test.Partest where

import Data.Graph
import Data.List ((\\))
import Data.Map (fromList, Map, (!))
import Data.Maybe (fromJust)
import Data.Tree (flatten)
import Test.QuickCheck

data BNF = BChoices | BSeqs | BTerm String
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

bterm :: String -> Integer -> BNF'
bterm s x = (BTerm s, x, [])

bsqes :: [BNF'] -> BNF'
bsqes bs = (BSeqs, 0, map (\(b, _, _) -> 0) bs)

es :: [BNF']
es =
  [ (BTerm "[singleTerm]", 0, [])
  , (BTerm "[seqTerm1]", 1, [])
  , (BTerm "[seqTerm2]", 2, [])
  , (BSeqs, 3, [1, 2])
  , (BTerm "[choicesTerm1]", 4, [])
  , (BTerm "[choicesTerm2]", 5, [])
  , (BChoices, 6, [4, 5])
  , (BTerm "[recursiveTerm1]", 7, [])
  , (BTerm "[recursiveTerm2]", 8, [])
  , (BChoices, 9, [10, 11])
  , (BChoices, 10, [9, 7])
  , (BChoices, 11, [9, 8])
  ]

ms :: Map Integer BNF'
ms =
  fromList
    [ (0, (BTerm "[singleTerm]", 0, []))
    , (1, (BTerm "[seqTerm1]", 1, []))
    , (2, (BTerm "[seqTerm2]", 2, []))
    , (3, (BSeqs, 3, [1, 2]))
    , (4, (BTerm "[choicesTerm1]", 4, []))
    , (5, (BTerm "[choicesTerm2]", 5, []))
    , (6, (BChoices, 6, [4, 5]))
    , (7, (BTerm "[recursiveTerm1]", 7, []))
    , (8, (BTerm "[recursiveTerm2]", 8, []))
    , (9, (BChoices, 9, [10, 11]))
    , (10, (BChoices, 10, [9, 7]))
    , (11, (BChoices, 11, [9, 8]))
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


r1 = scc graph

r2 = stronglyConnComp es

r3 = flatten $ head r1

isSingle :: [a] -> Bool
isSingle [_] = True
isSingle _ = False

ts :: [[Vertex]]
nts :: [[Vertex]]
(ts, nts) = filter' isSingle $ map flatten (scc graph)

terminals = map (second . nodeFromVertex . head) ts

isTerminal' x = x `elem` terminals

second :: (a, b, c) -> b
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

newtype Sym = Sym Symbol
  deriving (Show, Ord, Eq)

data Expr = Choices [Sym] | Seqs [Sym] | Term String
  deriving (Show, Ord, Eq)

type Symbol = String

data Rule = Rule Symbol Expr
  deriving (Show, Ord, Eq)

singleTerm = Rule "singleTerm" (Term "[singleTerm]")

seqTerm1 = Rule "seqTerm1" (Term "[seqTerm1]")

seqTerm2 = Rule "seqTerm2" (Term "[seqTerm2]")

seqTerm = Rule "seqTerm" $ Seqs [Sym "seqTerm1", Sym "seqTerm2"]

choicesTerm1 = Rule "choicesTerm1" (Term "[choicesTerm1]")

choicesTerm2 = Rule "choicesTerm2" (Term "[choicesTerm2]")

choicesTerm = Rule "choicesTerm" $ Choices [Sym "choicesTerm1", Sym "choicesTerm2"]

recursiveTerm1' = Rule "recursiveTerm1'" (Term "[recursiveTerm1]")

recursiveTerm2' = Rule "recursiveTerm2'" (Term "[recursiveTerm2]")

recursiveTerm = Rule "recursiveTerm" $ Choices [Sym "recursiveTerm1", Sym "recursiveTerm2"]

recursiveTerm1 = Rule "recursiveTerm1" (Choices [Sym "recursiveTerm", Sym "recursiveTerm1'"])

recursiveTerm2 = Rule "recursiveTerm2" (Choices [Sym "recursiveTerm", Sym "recursiveTerm2'"])


compile :: [Rule] -> [(BNF, Integer, [Integer])]
compile rs = zip3 bs is es
  where
    m = assignIds rs
    bs :: [BNF] = map compileExpr rs
    es :: [[Integer]] = map (`compileEdge` m) rs
    is :: [Integer] = map (`compileId` m) rs

inverseMap :: [(BNF, Integer, [Integer])] -> Map Integer BNF'
inverseMap = fromList . map (\(b, i, xs) -> (i, (b, i, xs)))

assignIds :: [Rule] -> Map Symbol Integer
assignIds rs = fromList $ zip (map symbol rs) [0 ..]

symbol :: Rule -> Symbol
symbol (Rule s _) = s

compileExpr :: Rule -> BNF
compileExpr (Rule _ (Choices _)) = BChoices
compileExpr (Rule _ (Seqs _)) = BSeqs
compileExpr (Rule _ (Term s)) = BTerm s

compileEdge :: Rule -> Map Symbol Integer -> [Integer]
compileEdge (Rule _ expr) m = map (m !) (flattenExpr expr)

flattenExpr :: Expr -> [Symbol]
flattenExpr (Seqs ss) = map flattenSym ss
flattenExpr (Choices cs) = map flattenSym cs
flattenExpr (Term s) = []

flattenSym :: Sym -> Symbol
flattenSym (Sym s) = s

compileId :: Rule -> Map Symbol Integer -> Integer
compileId (Rule s _) m = m ! s

newtype FTerm = FTerm String

instance Show FTerm where
  show (FTerm s) = s

newtype FSeq = FSeq [FTerm]

instance Show FSeq where
  show (FSeq fs) = concatMap show fs

concatF :: [FSeq] -> FSeq
concatF = FSeq . concatMap (\(FSeq fs) -> fs)

gen :: BNF' -> Map Integer BNF' -> Int -> Gen FSeq
gen (BTerm s, _, _) _ _ = return $ FSeq [FTerm s]
gen (BSeqs, x, xs) m i = do
  fs <- mapM (\x' -> gen (m ! x') m (i - 1)) xs
  return $ concatF fs
gen (BChoices, x, xs) m i
  | i < 1 = do
    let ts = filter isTerminal' xs
    case ts of 
      [] -> do
        t <- elements xs
        gen (m ! t) m (i - 1)
      _ -> do
        t <- elements ts
        gen (m ! t) m (i - 1)

  | otherwise = do
    j <- elements xs
    gen (m ! j) m (i - 1)

gen' :: [BNF'] -> Int -> Gen FSeq
gen' bs i = do
  j <- elements bs
  gen j ms i

g' x = sample $ gen' es x

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

es' = compile rules

ms' = inverseMap es'

