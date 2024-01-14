{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Tree where

import Data.List
import System.Random

newtype Knot = MkKnot ((KPoint, KPoint), (KPoint, KPoint))

instance Show Knot where
  show :: Knot -> String
  show (MkKnot ((a1, a2), (b1, b2))) = "{(" ++ show a1 ++ "," ++ show a2 ++ "),(" ++ show b1 ++ "," ++ show b2 ++ ")}"

data Tree a
  = Leaf a
  | Node (Tree a) Knot (Tree a)
  deriving (Show)

depth :: Tree a -> Integer
depth (Leaf x) = 1
depth (Node a _ c) = 1 + max (depth a) (depth c)

-- mapTree :: a -> Tree (a -> a) -> [a]
-- mapTree _ Empty = []
-- mapTree x (Node Empty f Empty) = [f x]
-- mapTree x (Node g f Empty) = mapTree (f x) g
-- mapTree x (Node g f h) = mapTree (f x) g ++ mapTree (f x) h

-- foldTree :: (b -> a -> b -> b) -> b -> Tree a -> b
-- foldTree _ nv Empty = nv
-- foldTree f nv (Node l x r) =
--  f
--    (foldTree f nv l)
--    x
--    (foldTree f nv r)

leftPoints :: [Split KPoint] -> [KPoint]
leftPoints ((L p) : ps) = p : leftPoints ps
leftPoints (p : ps) = leftPoints ps
leftPoints [] = []

rightPoints :: [Split KPoint] -> [KPoint]
rightPoints ((R p) : ps) = p : rightPoints ps
rightPoints (p : ps) = rightPoints ps
rightPoints [] = []

insertPoints :: [Split KPoint] -> Knot -> Tree [KPoint]
insertPoints l k = Node (Leaf $ leftPoints l) k (Leaf $ rightPoints l)

newtype KPoint = MkKPoint [Integer]

instance Eq KPoint where
  (==) :: KPoint -> KPoint -> Bool
  (MkKPoint (x : xs)) == (MkKPoint (y : ys)) = (x == y) && MkKPoint xs == MkKPoint ys
  (MkKPoint []) == (MkKPoint []) = True

instance Ord KPoint where
  (<) :: KPoint -> KPoint -> Bool
  (MkKPoint (x : xs)) < (MkKPoint (y : ys))
    | x < y = MkKPoint xs < MkKPoint ys
    | otherwise = False
  (MkKPoint []) < (MkKPoint []) = True
  (<=) :: KPoint -> KPoint -> Bool
  (MkKPoint (x : xs)) <= (MkKPoint (y : ys))
    | x <= y = MkKPoint xs <= MkKPoint ys
    | otherwise = False
  (MkKPoint []) <= (MkKPoint []) = True
  (>=) :: KPoint -> KPoint -> Bool
  (MkKPoint (x : xs)) >= (MkKPoint (y : ys))
    | x >= y = MkKPoint xs >= MkKPoint ys
    | otherwise = False
  (MkKPoint []) >= (MkKPoint []) = True
  (>) :: KPoint -> KPoint -> Bool
  (MkKPoint (x : xs)) > (MkKPoint (y : ys))
    | x > y = MkKPoint xs > MkKPoint ys
    | otherwise = False
  (MkKPoint []) > (MkKPoint []) = True
  compare :: KPoint -> KPoint -> Ordering
  compare p1 p2
    | p1 > p2 = GT
    | p2 < p1 = LT
    | otherwise = EQ

-- smallerThanKPoint :: KPoint -> KPoint -> Bool
-- smallerThanKPoint (MkKPoint (x : xs)) (MkKPoint (y : ys))
--  | x < y = smallerThanKPoint (MkKPoint xs) (MkKPoint ys)
--  | otherwise = False
-- smallerThanKPoint (MkKPoint []) (MkKPoint []) = True
--
-- biggerThanKPoint :: KPoint -> KPoint -> Bool
-- biggerThanKPoint (MkKPoint (x : xs)) (MkKPoint (y : ys))
--  | x > y = biggerThanKPoint (MkKPoint xs) (MkKPoint ys)
--  | otherwise = False
-- biggerThanKPoint (MkKPoint []) (MkKPoint []) = True

instance Show KPoint where
  show :: KPoint -> String
  show p = "(" ++ helper p ++ ")"
    where
      helper (MkKPoint [x]) = show x
      helper (MkKPoint (x : xs)) = show x ++ "," ++ helper (MkKPoint xs)
      helper (MkKPoint []) = ""

elemByIndex :: [a] -> Integer -> Maybe a
elemByIndex (x : xs) 0 = Just x
elemByIndex (x : xs) i = elemByIndex xs (i - 1)
elemByIndex [] _ = Nothing

elemByIndexKPoint :: KPoint -> Integer -> Maybe Integer
elemByIndexKPoint (MkKPoint p) = elemByIndex p

data Split e = L e | R e
  deriving (Show)

eitherPoint :: [KPoint] -> (Integer, Integer) -> [Split KPoint]
eitherPoint ps (x, v) = map (xiCheck (x, v)) ps
  where
    xiCheck :: (Integer, Integer) -> KPoint -> Split KPoint
    xiCheck (xi, val) p@(MkKPoint poin) = case elemByIndex poin xi of
      Just a -> if a > val then R p else L p
      Nothing -> error "wrong index"

minMaxP :: [[Integer]] -> ([Integer], [Integer])
minMaxP l = (helperMin $ transpose l, helperMax $ transpose l)
  where
    helperMin :: [[Integer]] -> [Integer]
    helperMin l = case l of
      (x : xs) -> minimum x : helperMin xs
      [] -> []
    helperMax :: [[Integer]] -> [Integer]
    helperMax l = case l of
      (x : xs) -> maximum x : helperMax xs
      [] -> []

minMaxKPoint :: [KPoint] -> (KPoint, KPoint)
minMaxKPoint kps = case minMaxP $ kpointsToIntegers kps of
  (a, b) -> (MkKPoint a, MkKPoint b)

kpointsToIntegers :: [KPoint] -> [[Integer]]
kpointsToIntegers (MkKPoint p : ps) = p : kpointsToIntegers ps
kpointsToIntegers [] = []

splitLeaf :: Tree [KPoint] -> (Integer, Integer) -> Tree [KPoint]
splitLeaf t xi = case helper t xi of
  (Node (Leaf l) c (Leaf r)) -> Node (Leaf l) (MkKnot (minMaxKPoint l, minMaxKPoint r)) (Leaf r)
  Leaf c -> Leaf c
  where
    helper :: Tree [KPoint] -> (Integer, Integer) -> Tree [KPoint]
    helper (Leaf c) xi =
      let res = insertPoints (eitherPoint c xi) (MkKnot ((MkKPoint [0], MkKPoint [0]), (MkKPoint [0], MkKPoint [0])))
       in case res of
            Node (Leaf []) _ _ -> Leaf c
            Node _ _ (Leaf []) -> Leaf c
            _ -> res
    helper _ _ = error "not a leaf to split"

-- splitLeaf (Leaf [MkKPoint [1, 2, 3], MkKPoint [2, 2, 3], MkKPoint [3, 3, 3]]) (1, 2)
-- (splitTree (splitLeaf (Leaf [MkKPoint [1, 2, 3], MkKPoint [2, 1, 3], MkKPoint [3, 2, 5], MkKPoint[6, 3, 1]]) (1, 2)) (2, 3))

splitTree :: Tree [KPoint] -> (Integer, Integer) -> Tree [KPoint]
splitTree (Leaf c) xi = splitLeaf (Leaf c) xi
splitTree (Node l k r) xi = Node (splitTree l xi) k (splitTree r xi)

generateKPoint :: Int -> Integer -> (Integer, Integer) -> KPoint
generateKPoint seed dimension (minV, maxV) = MkKPoint $ take (fromIntegral dimension) $ randomRs (minV, maxV) (mkStdGen seed)

generateTreeList :: Integer -> Int -> Integer -> (Integer, Integer) -> Tree [KPoint]
generateTreeList n seed dimension interv = Leaf (helper n seed dimension interv)
  where
    helper :: Integer -> Int -> Integer -> (Integer, Integer) -> [KPoint]
    helper 0 _ _ _ = []
    helper n seed dimension interv = generateKPoint seed dimension interv : helper (n - 1) (seed + 1) dimension interv

-- generateTreeList 100 12 4 (0,99)

median :: [Integer] -> Integer
median x
  | odd n = head $ drop (n `div` 2) x'
  | even n = mean $ take 2 $ drop i x'
  where
    i = (length x' `div` 2) - 1
    x' = sort x
    n = length x

mean :: [Integer] -> Integer
mean xs = sum xs `div` (fromIntegral . length) xs

medianKPoints :: [KPoint] -> KPoint
medianKPoints ps = MkKPoint $ medianP $ kpointsToIntegers ps
  where
    medianP :: [[Integer]] -> [Integer]
    medianP l = helper $ transpose l
      where
        helper :: [[Integer]] -> [Integer]
        helper (x : xs) = median x : helper xs

meanKPoints :: [KPoint] -> KPoint
meanKPoints ps = MkKPoint $ meanP $ kpointsToIntegers ps
  where
    meanP :: [[Integer]] -> [Integer]
    meanP l = helper $ transpose l
      where
        helper :: [[Integer]] -> [Integer]
        helper (x : xs) = median x : helper xs

-- elemByIndexKPoint (medianKPoints ps) i

allLeaves :: Tree [KPoint] -> [KPoint]
allLeaves (Leaf ps) = ps
allLeaves (Node l c r) = allLeaves l ++ allLeaves r

splitTreeNTimesAutoRule :: Integer -> Int -> Tree [KPoint] -> Tree [KPoint]
splitTreeNTimesAutoRule dimensions n t = helper n t 0
  where
    helper 0 t _ = t
    helper n t i = case elemByIndexKPoint (meanKPoints $ allLeaves t) i of
      Nothing -> error "splitTreeNTimesAutoRule has a problem1"
      Just a -> helper (n - 1) (splitTree t (i, a)) (mod (i + 1) dimensions)

-- splitTreeNTimesAutoRule 4 10 (generateTreeList 200 12 4 (0, 99))

searchTree :: Tree [KPoint] -> (KPoint, KPoint) -> [KPoint]
searchTree (Node l (MkKnot ((minVL, maxVL), (minVR, maxVR))) r) (a, b)
  | (minVL > b || maxVL < a) && (minVR > b || maxVR < a) = []
  | minVL > b || maxVL < a = searchTree r (a, b)
  | minVR > b || maxVR < a = searchTree l (a, b)
  | otherwise = searchTree l (a, b) ++ searchTree r (a, b)
--  if smallerThanOrEqKPoint a maxVL
--    && biggerThanOrEqKPoint b minVL
--    then
--      searchTree l (a, b)
--        ++ searchTree r (a, b)
--    else []
searchTree (Leaf (p : ps)) (a, b) =
  if p >= a && p <= b
    then p : searchTree (Leaf ps) (a, b)
    else searchTree (Leaf ps) (a, b)
searchTree (Leaf []) (a, b) = []

distance :: KPoint -> KPoint -> Float
distance a b = sqrt $ fromIntegral $ helper a b
  where
    helper (MkKPoint (x : xs)) (MkKPoint (y : ys)) = (x - y) ^ 2 + helper (MkKPoint xs) (MkKPoint ys)
    helper (MkKPoint []) (MkKPoint []) = 0

closestNeighborInBranch :: Tree [KPoint] -> KPoint -> KPoint
closestNeighborInBranch (Node l (MkKnot ((i1, i2), (j1, j2))) r) p
  | p <= i2 = closestNeighborInBranch l p
  | otherwise = closestNeighborInBranch r p
closestNeighborInBranch (Leaf []) _ = error "(Leaf []) in closestNeighborInBranch"
closestNeighborInBranch l@(Leaf (x : xs)) p = helper l p x
  where
    helper :: Tree [KPoint] -> KPoint -> KPoint -> KPoint
    helper (Leaf (l : ls)) p b
      | distance p l < distance p b = helper (Leaf ls) p l
      | otherwise = helper (Leaf ls) p b
    helper (Leaf []) p b = b

pp :: (Show a) => Tree a -> IO () -- Взето от: https://ardumont.github.io/fun-with-binary-search-tree?fbclid=IwAR3h5qHtz-i14oUKOplw6NJiGwyEQMeDCcgJeE_cXbo5u4GKB0FH2gNo56g
pp = mapM_ putStrLn . treeIndent
  where
    treeIndent (Leaf a) = ["---" ++ show a]
    treeIndent (Node lb c rb) =
      ["--" ++ show c]
        ++ map ("  |" ++) ls
        ++ ("  `" ++ r)
        : map ("   " ++) rs
      where
        (r : rs) = treeIndent rb
        ls = treeIndent lb

pp1 :: (Show a) => Tree a -> IO ()
pp1 = mapM_ putStrLn . treeIndent
  where
    treeIndent (Leaf a) = ["---" ++ show a]
    treeIndent (Node lb c rb) =
      ["--+"]
        ++ map ("  |" ++) ls
        ++ ("  `" ++ r)
        : map ("   " ++) rs
      where
        (r : rs) = treeIndent rb
        ls = treeIndent lb

-- writeFile "TreeVisual.scm" (showTree (splitTree (splitLeaf (Leaf [MkKPoint [1, 2, 3], MkKPoint [2, 1, 3], MkKPoint [3, 2, 5], MkKPoint[6, 3, 1]]) (1, 2)) (2, 3)) 1)
-- split tree till there is only one element in a leaf

closestNeighbor :: Tree [KPoint] -> KPoint -> KPoint
closestNeighbor = undefined

sphereIntersectsBox :: KPoint -> KPoint -> KPoint -> KPoint -> Bool
sphereIntersectsBox o r a b = undefined

-- main :: IO ()