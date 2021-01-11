module HW1_3 where

data Tree a = Node { _Left --ok
                   , _Central
                   , _Right :: Tree a
                   , value
                   , minValue
                   , maxValue :: a
                   }
            | EmptyTree deriving Show
instance Functor Tree where --ok
    fmap f EmptyTree = EmptyTree
    fmap f t = Node (fmap f (_Left t)) (fmap f (_Central t)) (fmap f (_Right t)) (f (value t)) (f (minValue t)) (f (maxValue t))

isEmptyTree :: Tree a -> Bool
isEmptyTree EmptyTree = True
isEmptyTree t = False

isLeaf :: Tree a -> Bool
isLeaf t = isEmptyTree (_Left t) && isEmptyTree (_Central t) && isEmptyTree (_Right t)

seek :: Ord a => a -> Tree a -> Maybe a --ok
seek v t
    | isEmptyTree t || isLeaf t && value t /= v = Nothing
    | value t == v = Just v
    | not (isEmptyTree (_Left t)) && maxValue (_Left t) >= v = seek v (_Left t)
    | not (isEmptyTree (_Central t)) && maxValue (_Central t) >= v = seek v (_Central t)
    | otherwise = seek v (_Right t)

minIfLeftIfCentralElseV :: Tree a -> a --gives min (_Left, _Central, node)
minIfLeftIfCentralElseV t
    | not (isEmptyTree (_Left t)) = minValue (_Left t)
    | not (isEmptyTree (_Central t)) = minValue (_Central t)
    | otherwise = value t

maxIfRightElseV :: Tree a -> a --gives max (_Right, node)
maxIfRightElseV t
    | not (isEmptyTree (_Right t)) = maxValue (_Right t)
    | otherwise = value t

delete :: Ord a => a -> Tree a -> Tree a --ok
delete v t
    | isEmptyTree t || isLeaf t && value t /= v = t
    | isLeaf t && value t == v = EmptyTree
    | value t == v = if not (isEmptyTree (_Central t))
                         then let newTree = t {_Central = delete (maxValue (_Central t)) (_Central t)}
                              in newTree {value = maxValue (_Central t), minValue = minIfLeftIfCentralElseV newTree, maxValue = maxIfRightElseV newTree}
                         else if not (isEmptyTree (_Right t))
                                  then let newTree = t {_Right = delete (minValue (_Right t)) (_Right t)}
                                       in newTree {value = minValue (_Right t), minValue = minIfLeftIfCentralElseV newTree, maxValue = maxIfRightElseV newTree}
                                  else let newTree = t {_Left = delete (maxValue (_Left t)) (_Left t)}
                                       in newTree {value = maxValue (_Left t), minValue = minIfLeftIfCentralElseV newTree, maxValue = maxIfRightElseV newTree}
    | not (isEmptyTree (_Left t)) && maxValue (_Left t) >= v =
        let newTree = t {_Left = delete v (_Left t)}
        in newTree {minValue = minIfLeftIfCentralElseV newTree, maxValue = maxIfRightElseV newTree}
    | not (isEmptyTree (_Central t)) && maxValue (_Central t) >= v =
        let newTree = t {_Central = delete v (_Central t)}
        in newTree {minValue = minIfLeftIfCentralElseV newTree, maxValue = maxIfRightElseV newTree}
    | otherwise =
        let newTree = t {_Right = delete v (_Right t)}
        in newTree {minValue = minIfLeftIfCentralElseV newTree, maxValue = maxIfRightElseV newTree}

add :: Ord a => a -> Tree a -> Tree a --ok (could this add an existing element?)
add v t
    | isEmptyTree t = Node EmptyTree EmptyTree EmptyTree v v v
    | value t == v = t
    | isLeaf t && value t > v = t {_Left = add v EmptyTree, minValue = v, maxValue = value t}
    | isLeaf t && value t < v = t {_Right = add v EmptyTree, minValue = value t, maxValue = v}
    | not (isEmptyTree (_Left t)) && maxValue (_Left t) >= v = t {_Left = add v (_Left t), minValue = min (minValue t) v, maxValue = max (maxValue t) v}
    | not (isEmptyTree (_Central t)) && maxValue (_Central t) >= v = t {_Central = add v (_Central t), minValue = min (minValue t) v, maxValue = max (maxValue t) v}
    | otherwise = t {_Right = add v (_Right t), minValue = min (minValue t) v, maxValue = max (maxValue t) v}
{-
merge :: Ord a => Tree a -> Tree a -> Tree a
merge

balance :: Ord a => Tree a -> Tree a
balance
-}
theRealKmin :: Ord a => Int -> Tree a -> (Int, [a])
theRealKmin k t
    | k == 0 || isEmptyTree t = (0, [])
    | isLeaf t = (1, [value t])
    | otherwise = let (kl, al) = theRealKmin k (_Left t)
                  in if kl == k
                     then (kl, al)
                     else let (kc, ac) = theRealKmin (k - kl) (_Central t)
                          in if kc == k - kl
                             then (k, al ++ ac)
                             else let (kr, ar) = theRealKmin (k - kl - kc - 1) (_Right t)
                                  in (kl + kc + 1 + kr, al ++ ac ++ [value t] ++ ar)

kmin :: Ord a => Int -> Tree a -> [a] --ok
kmin k t =
    let (_, a) = theRealKmin k t
    in a

theRealKmax :: Ord a => Int -> Tree a -> (Int, [a])
theRealKmax k t
    | k == 0 || isEmptyTree t = (0, [])
    | isLeaf t = (1, [value t])
    | otherwise = let (kr, ar) = theRealKmax k (_Right t)
                  in if kr == k
                     then (kr, ar)
                     else let (kc, ac) = theRealKmax (k - kr - 1) (_Central t)
                          in if kc == k - kr - 1
                             then (k, ar ++ [value t] ++ ac)
                             else let (kl, al) = theRealKmax (k - kr - 1 - kc) (_Left t)
                                  in (kr + 1 + kc + kl, ar ++ [value t] ++ ac ++ al)

kmax :: Ord a => Int -> Tree a -> [a]
kmax k t =
    let (_, a) = theRealKmax k t
    in a

data N = Z | S N deriving Show
instance Eq N where
    (==) Z Z = True
    (==) Z b = False
    (==) a Z = False
    (==) (S a) (S b) = a == b
    (/=) a b = not (a == b)
instance Ord N where --ok
    compare Z Z = EQ
    compare Z b = LT
    compare a Z = GT
    compare (S a) (S b) = compare a b
    (<=) a b = compare a b /= GT
    (<)  a b = compare a b == LT
    (>=) a b = compare a b /= LT
    (>)  a b = compare a b == GT
    max a b
        | a >= b = a
        | otherwise = b
    min a b
        | a < b = a
        | otherwise = b

natSum :: N -> N -> N --ok
natSum a Z = a
natSum Z b = b
natSum (S a) b = S (natSum a b)

natMult :: N -> N -> N --ok
natMult _ Z = Z
natMult Z _ = Z
natMult (S a) b = natSum b (natMult a b)

repeatN :: N -> N -> N -> N -> N
repeatN n a Z result = result
repeatN (S n) a (S b) result = repeatN (S n) a b (superNat n a result)

superNat :: N -> N -> N -> N --ok
superNat Z a b = natSum a b
superNat (S Z) a Z = Z
superNat n a Z = S Z
superNat n a (S b) = repeatN n a b a
