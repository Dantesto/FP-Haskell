module HW1_2 where

import Data.List
import Text.Read
import Data.Char
import Data.Maybe

isSorted :: Ord a => [a] -> Int -> Int -> Bool
isSorted a k n
    | k == n = True
    | a !! (k - 1) > a !! k = False
    | otherwise = isSorted a (k + 1) n

isaSorted :: Ord a => [a] -> Int -> Int -> Bool
isaSorted a k n
    | k == n = True
    | a !! (k - 1) < a !! k = False
    | otherwise = isaSorted a (k + 1) n

antisort :: Ord a => [a] -> [a] --ok
antisort a = if isSorted a 1 (length a) || isaSorted a 1 (length a)
                 then drop (length a `div` 2) a ++ take (length a `div` 2) a
                 else a

antiprimes :: Int -> [Integer] --ok
antiprimes k = map toInteger (take k ([1..k * 3] \\ eratos [2..k * 3]))
    where eratos [] = []
          eratos (p:xs) = p : eratos (xs \\ [p * p, p * p + p..k * 3])

absoluteMinus :: Eq a => [a] -> [a] -> [a] --[1,1,1,2] - [1] = [2]
absoluteMinus a b =
    let minus = a \\ b
    in if length minus == length (minus \\ b)
           then minus
           else absoluteMinus minus b

antiunion :: Eq a => [a] -> [a] -> [a] --ok
antiunion a b = absoluteMinus a b ++ absoluteMinus b a

antimerge :: Eq a => [a] -> [(Int, a)] --ok
antimerge a =
    let l = length a
        b = [x | x <- a, x /= head a]
    in if l == 0
           then []
           else [(l - length b, head a)] ++ antimerge b

theRealAntiintercalate :: Eq a => a -> Int -> [a] -> [(Int, a)]
theRealAntiintercalate prev cnt a
    | length a == 0 = [(cnt, prev)]
    | head a == prev = theRealAntiintercalate prev (cnt + 1) (tail a)
    | otherwise = [(cnt, prev)] ++ theRealAntiintercalate (head a) 1 (tail a)

antiintercalate :: Eq a => [a] -> [(Int, a)] --ok
antiintercalate [] = [] --kostyl'
antiintercalate a = theRealAntiintercalate (head a) 1 (tail a)

antiantiintercalate :: [(Int, a)] -> [a] --ok
antiantiintercalate [] = []
antiantiintercalate a =
    let h = head a
    in replicate (fst h) (snd h) ++ antiantiintercalate (tail a)

getNumberOrNot :: String -> Maybe Integer --ok
getNumberOrNot s = readMaybe [c | c <- s, not (isSpace c)]
{-
checkPrefix :: String -> Bool
checkPrefix s = 

checkDomain :: String -> Bool
checkDomain s = 

getMailDomainorNot :: String -> Maybe String --a-z, 0-9, (_, ., - followed by letter or number). a-z, 0-9, - (one . followed by 2 or more letters)
getMailDomainorNot s =
    let s' = s ++ "@"
        atInd = elemIndex '@' s'
        prefix = take atInd s'
        domain = take (length s' - atInd - 1) (drop (atInd + 1) s')
    in if checkPrefix && checkDomain
           then Just s
           else Nothing
-}

maybeMaybeMaybeMabyeMaybeMaybeMaybeOrNot :: Maybe (Maybe (Maybe (Maybe (Maybe (Maybe a))))) -> a -> a --ok
maybeMaybeMaybeMabyeMaybeMaybeMaybeOrNot a b = fromJust (fromJust (fromJust (fromJust (fromJust (fromJust a)))))

pack4List :: [a] -> [(a, a, a, a)]
pack4List [] = []
pack4List (a:(b:(c:(d:l)))) = [(a, b, c, d)] ++ pack4List l

stupidTraverse :: [Maybe a] -> Maybe [(a, a, a, a)] --ok
stupidTraverse a =
    let b = [fromJust x | x <- a, isJust x]
    in if length b `mod` 4 == 0
           then Just (pack4List b)
           else Nothing

getEdgeInd :: [(Int, Int)] -> Int -> Int -> [Bool] -> Int -> Int
getEdgeInd e i j visited curInd
    | curInd >= length e = -1
    | fst (e !! curInd) == i && not (visited !! curInd) = curInd
    | otherwise = getEdgeInd e i j visited (curInd + 1)

theRealDfs :: [(Int, Int)] -> Int -> Int -> [Bool] -> Bool
theRealDfs e i j visited
    | i == j = True
    | otherwise =
        let nextMove = getEdgeInd e i j visited 0
            nextI = snd (e !! nextMove)
            updatedVisited = take nextMove visited ++ [True] ++ drop (nextMove + 1) visited
        in if nextMove /= -1 && not (visited !! nextMove)
               then theRealDfs e nextI j updatedVisited || theRealDfs e i j updatedVisited
               else False

dfs :: [(Int, Int)] -> Int -> Int -> Bool --ok
dfs e i j = theRealDfs e i j (replicate (length e) False)

myFst :: (a, b, c) -> a
myFst (a, b, c) = a

mySnd :: (a, b, c) -> b
mySnd (a, b, c) = b

myThd :: (a, b, c) -> c
myThd (a, b, c) = c

getEdgeInd2 :: (Num a, Ord a) => [(Int, Int, a)] -> Int -> Int -> [Bool] -> Int -> Int
getEdgeInd2 e i j visited curInd
    | curInd >= length e = -1
    | myFst (e !! curInd) == i && not (visited !! curInd) = curInd
    | otherwise = getEdgeInd2 e i j visited (curInd + 1)

getWaysList :: (Num a, Ord a) => [(Int, Int, a)] -> Int -> Int -> [Bool] -> a -> [a]
getWaysList e i j visited curWay
    | i == j = [curWay]
    | otherwise =
        let nextMove = getEdgeInd2 e i j visited 0
            nextI = mySnd (e !! nextMove)
            updatedCurWay = curWay + myThd (e !! nextMove)
            updatedVisited = take nextMove visited ++ [True] ++ drop (nextMove + 1) visited
        in if nextMove /= -1 && not (visited !! nextMove)
               then (getWaysList e nextI j updatedVisited updatedCurWay) ++ (getWaysList e i j updatedVisited curWay)
               else []

getSecondMin :: (Num a, Ord a) => [a] -> a -> a -> a
getSecondMin a min1 min2
    | length a == 0 = min2
    | head a <= min1 = getSecondMin (tail a) (head a) min1
    | head a > min1 && head a < min2 = getSecondMin (tail a) min1 (head a)
    | otherwise = getSecondMin (tail a) min1 min2

fatWay :: (Num a, Ord a) => [(Int, Int, a)] -> Int -> Int -> a --ok
fatWay e i j =
    let waysList = getWaysList e i j (replicate (length e) False) 0
        min1 = min (waysList !! 0) (waysList !! 1)
        min2 = max (waysList !! 0) (waysList !! 1)
    in if length waysList < 2
           then fromInteger (negate 1)
           else getSecondMin (drop 2 waysList) min1 min2
