
myPlus :: Int -> Int -> Int
myPlus n m =
  if n == 0
    then m
    else succ (myPlus (pred n) m)

myMulti :: Int -> Int -> Int
myMulti x y
  | x == 0 = 0
  | x == 1 = y
  | otherwise = (+) (myMulti (pred x) y) y

fastPow :: Integer -> Integer -> Integer
fastPow x n =
  if even n
    then (x * x) ^ div n 2
    else x ^ n

isOdd :: Integer -> Bool
isOdd n =
  not (n == 0)
    && isEven (n - 1)

isEven :: Integer -> Bool
isEven n =
  n == 0 || isOdd (n - 1)