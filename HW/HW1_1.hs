module HW1_1 where

f1_1 :: Int -> Int --ok
f1_1 x = x

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

f2_1 :: Int -> Bool --ok
f2_1 x =
    let y = abs x
    in
    if y > 1
        then null [d | d <- [2..isqrt y], y `mod` d == 0]
        else False

f3_1 :: Bool -> Bool -> Int --ok
f3_1 x y = (if x then 1 else 0) + (if y then 1 else 0)

f4_1 :: Int -> Int --ok
f4_1 x =
    let y = abs x
        d1 = [d | d <- [2..isqrt y], y `mod` d == 0]
        d2 = map (\d -> y `div` d) d1
    in sum (d1 ++ d2) + 1

f5_1 :: Int -> Int --ok
f5_1 x = if f4_1 (x + 1) == x + 1
             then x + 1
             else f5_1 (x + 1)

f6_1 :: Integer -> Integer --ok
f6_1 x = toInteger (f5_1 (fromIntegral x))

f7_1 :: Int -> Int -> Int --ok
f7_1 m n
    | m == 0 = n + 1
    | m > 0 && n == 0 = f7_1 (m - 1) 1
    | otherwise = f7_1 (m - 1) (f7_1 m (n - 1))

f8_1 :: Int -> Int -> Integer --ok
f8_1 m n = toInteger (f7_1 m n) 

--f9_1 :: Double -> Double -> Double -> Double -> (Double, Double, Double)
--f9_1 a b c d = 

f10_1 :: Double -> Double -> (Double, Double) --hz
f10_1 x y = (fromIntegral (floor (x / y)), x - y * fromIntegral (floor (x / y)))

brounckerFormula :: Int -> Int -> Double
brounckerFormula k n
    | n == k = 2.0 --what to do now?
    | n == 1 = 1.0 / (1 + 1 / brounckerFormula k (n + 1))
    | otherwise = 2.0 + fromIntegral (n * 2 - 1)^2 / brounckerFormula k (n + 1)

f11_1 :: Int -> Double --ok?
f11_1 k = brounckerFormula k 1

f12_1 :: Int --lol?
f12_1 = 2
