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

data Rule = Rule Sym Expr
  deriving (Show, Ord, Eq)

singleTerm :: Rule
singleTerm = Rule (Sym "singleTerm") (Term "[singleTerm]")

seqTerm1 :: Rule
seqTerm1 = Rule (Sym "seqTerm1") (Term "[seqTerm1]")
seqTerm2 :: Rule

seqTerm :: Rule
seqTerm2 = Rule (Sym "seqTerm2") (Term "[seqTerm2]")

seqTerm = Rule (Sym "seqTerm") $ Seqs [Sym "seqTerm1", Sym "seqTerm2"]

choicesTerm1 :: Rule
choicesTerm1 = Rule (Sym "choicesTerm1") (Term "[choicesTerm1]")

choicesTerm2 :: Rule
choicesTerm2 = Rule (Sym "choicesTerm2") (Term "[choicesTerm2]")

choicesTerm :: Rule
choicesTerm = Rule (Sym "choicesTerm") $ Choices [Sym "choicesTerm1", Sym "choicesTerm2"]

recursiveTerm1' :: Rule
recursiveTerm1' = Rule (Sym "recursiveTerm1'") (Term "[recursiveTerm1]")

recursiveTerm2' :: Rule
recursiveTerm2' = Rule (Sym "recursiveTerm2'") (Term "[recursiveTerm2]")

recursiveTerm :: Rule
recursiveTerm = Rule (Sym "recursiveTerm") $ Choices [Sym "recursiveTerm1", Sym "recursiveTerm2"]

recursiveTerm1 :: Rule
recursiveTerm1 = Rule (Sym "recursiveTerm1") (Choices [Sym "recursiveTerm", Sym "recursiveTerm1'"])

recursiveTerm2 :: Rule
recursiveTerm2 = Rule (Sym "recursiveTerm2") (Choices [Sym "recursiveTerm", Sym "recursiveTerm2'"])


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
symbol (Rule s _) = s

compileExpr :: Rule -> BNF
compileExpr (Rule sym (Choices _)) = BChoices sym
compileExpr (Rule sym (Seqs _)) = BSeqs sym
compileExpr (Rule sym (Term s)) = BTerm s sym

compileEdge :: Rule -> Map Sym Integer -> [Integer]
compileEdge (Rule _ expr) m = map (m !) (flattenExpr expr)

flattenExpr :: Expr -> [Sym]
flattenExpr (Seqs ss) = ss
flattenExpr (Choices cs) = cs
flattenExpr (Term _) = []


compileId :: Rule -> Map Sym Integer -> Integer
compileId (Rule s _) m = m ! s

newtype FTerm = FTerm String

instance Show FTerm where
  show (FTerm s) = s

newtype FSeq = FSeq [FTerm]

data Res = RTerm String Sym | RSeq Sym | RChoices Sym
  deriving (Show)

-- need this newtype wrapper for a custom show instance
newtype TRes = TRes (Tree Res)

instance Show TRes where
  show (TRes t) = showTree t

showTree :: Tree Res -> String
showTree (Node (RTerm s _) _) = s
showTree (Node (RSeq _) ts) = concatMap showTree ts
showTree (Node (RChoices _) cs) = concatMap showTree cs

instance Show FSeq where
  show (FSeq fs) = concatMap show fs

concatF :: [FSeq] -> FSeq
concatF = FSeq . concatMap (\(FSeq fs) -> fs)

unwrap :: TRes -> Tree Res
unwrap (TRes t) = t


gen :: BNF' -> Map Integer BNF' -> Int -> Gen TRes
gen (BTerm s sym, _, _) _ _ = return $ TRes $ Node (RTerm s sym) []
gen (BSeqs sym, _, xs) m i = do
  fs <- mapM (\x' -> gen (m ! x') m (i - 1)) xs
  return $ TRes (Node (RSeq sym) (map unwrap fs))
gen (BChoices sym, _, xs) m depth
  | depth < 1 = do
    let ts' = filter isTerminal' xs
    case ts' of 
      [] -> gen' xs
      _ -> gen' ts'
  | otherwise = gen' xs
  where
    gen' :: [Integer] -> Gen TRes
    gen' xss = do
      j <- elements xss
      res <- gen (m ! j) m (depth - 1)
      return $ TRes $ Node (RChoices sym) [unwrap res]


genAll :: [BNF'] -> Int -> Gen TRes
genAll bs i = do
  j <- elements bs
  gen j ms i

g' :: Int -> IO ()
g' x = sample $ genAll es x

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

