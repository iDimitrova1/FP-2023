import Data.List
import System.Random

newtype Knot = MkKnot (Integer, KPoint)

instance Show Knot where
  show :: Knot -> String
  show (MkKnot (i, p)) = "[" ++ show i ++ "]" ++ "{" ++ show p ++ "}"

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

instance Show KPoint where
  show :: KPoint -> String
  show p = "(" ++ helper p ++ ")"
    where
      helper (MkKPoint [x]) = show x
      helper (MkKPoint (x : xs)) = show x ++ "," ++ helper (MkKPoint xs)
      helper (MkKPoint []) = ""

data Tree a
  = Leaf a
  | Node (Tree a) Knot (Tree a)
  deriving (Show)

elemByIndex :: [a] -> Integer -> a
elemByIndex (x : xs) 0 = x
elemByIndex (x : xs) i = elemByIndex xs (i - 1)
elemByIndex [] _ = error "wrong index in elemByIndex"

-- partition :: (a -> Bool) -> [a] -> ([a], [a])
splitKPoints :: [KPoint] -> Knot -> ([KPoint], [KPoint])
splitKPoints ps k@(MkKnot (xi, MkKPoint c)) = partition helper ps
  where
    helper :: KPoint -> Bool
    helper (MkKPoint p) = elemByIndex p xi <= elemByIndex c xi

-- splitKPoints l (MkKnot (1, (MkKPoint [11,33,60])))

kpointsToIntegers :: [KPoint] -> [[Integer]]
kpointsToIntegers (MkKPoint p : ps) = p : kpointsToIntegers ps
kpointsToIntegers [] = []

medianIndex :: (Ord a) => [a] -> Integer
medianIndex xs = case elemIndex (elemByIndex (sort xs) (fromIntegral ((length xs - 1) `div` 2))) xs of
  Just a -> fromIntegral a
  Nothing -> error "outside of range in medianIndex"

makeKnotMidPt :: [KPoint] -> Integer -> Knot
makeKnotMidPt ps ix = MkKnot (ix, elemByIndex ps (medianIndex (elemByIndex (transpose $ kpointsToIntegers ps) ix)))

splitKPointsToTree :: [KPoint] -> Knot -> Tree [KPoint]
splitKPointsToTree [p] kn = Leaf [p]
splitKPointsToTree ps k@(MkKnot (ix, MkKPoint p)) = case splitKPoints ps k of
  ([], _) -> splitKPointsToTree ps (makeKnotMidPt ps (mod (ix + 1) (fromIntegral $ length p)))
  (_, []) -> splitKPointsToTree ps (makeKnotMidPt ps (mod (ix + 1) (fromIntegral $ length p)))
  (lps, rps) -> Node (Leaf lps) k (Leaf rps)

-- splitKPointsToTree ps (makeKnodMidPt ps (mod (ix + 1) (fromIntegral $ length p)))

splitLeaf :: Tree [KPoint] -> Knot -> Tree [KPoint]
splitLeaf t kn@(MkKnot (ix, k@(MkKPoint p))) =
  case t of
    Leaf [x] -> t
    -- Leaf [] -> error "trying to split an empty leaf"
    Leaf ps -> case splitKPointsToTree ps kn of
      Leaf a -> splitKPointsToTree ps (makeKnotMidPt ps (mod (ix + 1) (fromIntegral $ length p)))
      (Node (Leaf a) c (Leaf b)) -> Node (splitLeaf (Leaf a) (makeKnotMidPt a (mod (ix + 1) (fromIntegral $ length p)))) c (splitLeaf (Leaf b) (makeKnotMidPt b (mod (ix + 1) (fromIntegral $ length p))))

splitTree :: Tree [KPoint] -> Tree [KPoint]
splitTree = splitTreeIt 0

splitTreeIt :: Integer -> Tree [KPoint] -> Tree [KPoint]
splitTreeIt n (Leaf c) = case splitLeaf (Leaf c) (makeKnotMidPt c n) of
  Leaf a -> Leaf a
  tr -> splitTreeIt (n + 1) tr
splitTreeIt _ (Node a kn@(MkKnot (ix, MkKPoint p)) b) =
  Node
    (splitTreeIt (mod (ix + 1) (fromIntegral $ length p)) a)
    kn
    (splitTreeIt (mod (ix + 1) (fromIntegral $ length p)) b)

-- splitTree (Leaf [MkKPoint [1, 2, 3], MkKPoint [2, 1, 3], MkKPoint [3, 2, 5], MkKPoint[6, 3, 1]])
-- pp $ splitTree (Leaf $ nub $ generateKPoints 8 12 3 (1,99))

generateKPoint :: Int -> Integer -> (Integer, Integer) -> KPoint
generateKPoint seed dimension (minV, maxV) = MkKPoint $ take (fromIntegral dimension) $ randomRs (minV, maxV) (mkStdGen seed)

generateKPoints :: Int -> Int -> Integer -> (Integer, Integer) -> [KPoint]
generateKPoints 0 _ _ _ = []
generateKPoints a seed dimension (minV, maxV) = generateKPoint seed dimension (minV, maxV) : generateKPoints (a - 1) (seed + 1) dimension (minV, maxV)

-- nub :: Eq a => [a] -> [a] - removes duplicates
-- nub $ generateKPoints 8 12 3 (1,99)

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