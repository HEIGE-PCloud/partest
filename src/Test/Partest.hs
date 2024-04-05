module Test.Partest where

import Test.QuickCheck
import Data.Graph
import Data.Maybe (fromJust)
import Data.List ((\\))
import Data.Tree (flatten)

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
es = [
    (BTerm "singleTerm", 0, [])
  , (BTerm "seqTerm1", 1, [])
  , (BTerm "seqTerm2", 2, [])
  , (BSeqs, 3, [1, 2])
  , (BTerm "choicesTerm1", 4, [])
  , (BTerm "choicesTerm2", 5, [])
  , (BChoices, 6, [4, 5])
  , (BTerm "recursiveTerm1", 7, [])
  , (BTerm "recursiveTerm2", 8, [])
  , (BChoices, 9, [10, 11])
  , (BChoices, 10, [9, 7])
  , (BChoices, 11, [9, 8])
  ]

graph :: Graph
nodeFromVertex :: Vertex -> (BNF, Integer, [Integer])
vertexFromKey :: Integer -> Maybe Vertex
(graph, nodeFromVertex, vertexFromKey) = graphFromEdges es


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



-- BRule "singleTerm" = BTerm "singleTerm"
-- BRule "seqTerm1" = BTerm "seqTerm1"
-- BRule "seqTerm2" = BTerm "seqTerm2"
-- BRule "seqTerm" = BSeqs [BRule "seqTerm1", BRule "seqTerm2"]
-- BRule "choicesTerm1" = BTerm "choicesTerm1"
-- BRule "choicesTerm2" = BTerm "choicesTerm2"
-- BRule "choicesTerm" = BChoices [BRule "choicesTerm1", BRule "choicesTerm2"]
-- BRule "recursiveTerm1" = BChoices [BRule "recursiveTerm", BTerm "recursiveTerm1"]
-- BRule "recursiveTerm2" = BChoices [BRule "recursiveTerm", BTerm "recursiveTerm2"]
-- BRule "recursiveTerm" = BChoices [BRule "recursiveTerm1", BRule "recursiveTerm2"]


-- bType :: BNF
-- bType = BChoices [baseType, arrayType, pairType]

-- baseType :: BNF
-- baseType = BChoices [BTerm "int", BTerm "bool", BTerm "char", BTerm "string"]

-- arrayType :: BNF
-- arrayType = BSeqs [bType, BTerm "[", BTerm "]"]

-- pairType :: BNF
-- pairType =
--   BSeqs
--     [BTerm "pair", BTerm "(", pairElemType, BTerm ",", pairElemType, BTerm ")"]

-- pairElemType :: BNF
-- pairElemType = BChoices [baseType, arrayType, BTerm "pair"]

newtype FTerm = FTerm String

instance Show FTerm where
  show (FTerm s) = s

newtype FSeq = FSeq [FTerm]

instance Show FSeq where
  show (FSeq fs) = concatMap show fs

concatF :: [FSeq] -> FSeq
concatF = FSeq . concatMap (\(FSeq fs) -> fs)

-- gen :: BNF -> Int -> Gen FSeq
-- gen (BTerm s) _ = return $ FSeq [FTerm s]
-- gen (BSeqs bs) x = concatF <$> mapM (`gen` x) bs
-- gen (BChoices bs) x = elements bs >>= (`gen` x)

