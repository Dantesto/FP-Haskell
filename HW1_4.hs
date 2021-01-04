module HW1_4 where

import Data.Maybe
import Data.List.Split

apb :: IO () --ok
apb = do
    a <- readLn
    b <- readLn
    print (a + b)

theRealWhileNotZero :: IO Int
theRealWhileNotZero = do
    a <- readLn
    if a == 0
        then return 0
        else do
            b <- theRealWhileNotZero
            return (a + b)

whileNotZero :: IO () --ok
whileNotZero = do
    sum <- theRealWhileNotZero
    print sum

getRandomList :: Int -> Int -> [Int]
getRandomList seed k
    | k == 0 = []
    | otherwise =
        let newSeed = (seed * 73129 + 95121) `mod` 100000
        in [newSeed] ++ getRandomList newSeed (k - 1)

fakeRandom :: IO () --ok
fakeRandom = do
    seed <- readLn
    k <- readLn
    print (getRandomList seed k) --mapM_ print (getRandomList seed k)

data FS = File String String | Directory String [FS] deriving Show

isFile :: FS -> Bool
isFile (File _ _) = True
isFile _ = False

getName :: FS -> String
getName (File file content) = file
getName (Directory dir content) = dir

getContent :: FS -> [FS]
getContent fs@(File file content) = [fs]
getContent (Directory dir content) = content

twoDimensionToOneDimension :: [[a]] -> [a]
twoDimensionToOneDimension [] = []
twoDimensionToOneDimension (a:as) = a ++ twoDimensionToOneDimension as

stringListToString :: [String] -> String
stringListToString [] = ""
stringListToString (s:ss) = s ++ stringListToString ss

getChildParentList :: FS -> [(String, FS)]
getChildParentList (File _ _) = []
getChildParentList fs = map (\c -> (getName c, fs)) (getContent fs) ++ twoDimensionToOneDimension (map getChildParentList (getContent fs))
{-    let content = getContent fs
    in if length content /= 0
           then [(getName (head content), fs)] ++ getChildParentList (Directory (getName fs) (tail content)) ++ getChildParentList (head content)
           else []-}

updateChildParentList :: FS -> [(String, FS)] -> [(String, FS)]
updateChildParentList updatedFS cpList
    | getName (getParent (getName updatedFS) cpList) == getName updatedFS = [(getName updatedFS, updatedFS)] ++  getChildParentList updatedFS
    | otherwise =
        let parent = getParent (getName updatedFS) cpList
            updatedContent' = remove (getName updatedFS) (getContent parent) ++ [updatedFS]
            updatedFS' = Directory (getName parent) updatedContent'
        in updateChildParentList updatedFS' cpList

getParent :: String -> [(String, FS)] -> FS
getParent child (cp:cpList) = if child == fst cp
                                  then snd cp
                                  else getParent child cpList

contains :: String -> [FS] -> Bool
contains name [] = False
contains name (c:content) = getName c == name || contains name content

remove :: String -> [FS] -> [FS]
remove name [] = []
remove name (c:content)
    | getName c == name = content
    | otherwise = [c] ++ remove name content

getPath :: String -> ([String], Bool) --(path, isNormalPath). Normal path - path from root, weird path - path from current dir
getPath s =
    let s' = if last s /= '/' then s ++ "/" else s
    in if head s' == '/'
           then (endBy "/" (tail s'), True)
           else let weirdPath = endBy "/" s'
                in (tail weirdPath ++ [head weirdPath], False)

getFS :: FS -> [String] -> Maybe FS
getFS fs path
    | length path == 0 = Just fs
    | otherwise =
        let content = getContent fs
        in if length content == 0
               then Nothing
               else if getName (head content) /= head path
                        then getFS (Directory (getName fs) (tail content)) path
                        else getFS (Directory (getName (head content)) (getContent (head content))) (tail path)

runLs :: FS -> IO ()
runLs fs = print (getContent fs)

theRealRunFS :: [(String, FS)] -> FS -> IO ()
theRealRunFS cpList fs = do
    commands <- getLine
    let commandList = words commands
        fullFS = snd (head cpList)
    case head commandList of
        "exit" -> if length commandList == 1 --ok
                      then return ()
                      else do
                          putStrLn ("command " ++ head commandList ++ " require more/less arguments")
                          theRealRunFS cpList fs
        "ls" -> do --ok
            if length commandList == 1
                then runLs fs
                else if length commandList == 2
                         then let (path, isNormalPath) = getPath (commandList !! 1)
                                  newFS = if isNormalPath then getFS fullFS path else getFS fs path
                              in if isJust newFS
                                     then runLs (fromJust newFS)
                                     else putStrLn ("Directory " ++ (commandList !! 1) ++ " doesn't exist")
                         else putStrLn ("command " ++ head commandList ++ " require more/less arguments")
            theRealRunFS cpList fs
        "cd" -> if length commandList == 2 --ok
                    then if commandList !! 1 == ".."
                             then theRealRunFS cpList (getParent (getName fs) cpList)
                             else let newFS = if head (commandList !! 1) == '/'
                                                  then getFS fullFS (fst (getPath (commandList !! 1)))
                                                  else getFS fs (fst (getPath (commandList !! 1)))
                                  in if isJust newFS
                                         then theRealRunFS cpList (fromJust newFS)
                                         else do
                                             putStrLn ("Directory " ++ (commandList !! 1) ++ " doesn't exist")
                                             theRealRunFS cpList fs
                    else do
                        putStrLn ("command " ++ head commandList ++ " require more/less arguments")
                        theRealRunFS cpList fs
        "mkdir" -> if length commandList == 2 --ok
                       then let pathNdir = fst (getPath (commandList !! 1))
                                path = init pathNdir
                                dir = last pathNdir
                                newFS = if head (commandList !! 1) == '/'
                                            then getFS fullFS path
                                            else Just fs
                                updatedCpList = (updateChildParentList (Directory (getName (fromJust newFS)) ((getContent (fromJust newFS)) ++ [Directory dir []])) cpList)
                            in if isJust newFS
                                   then if not (contains dir (getContent (fromJust newFS)))
                                            then if getName (getParent (getName fs) updatedCpList) /= getName fs
                                                     then theRealRunFS updatedCpList (fromJust (getFS (getParent (getName fs) updatedCpList) [getName fs]))
                                                     else theRealRunFS updatedCpList (getParent (getName fs) updatedCpList)
                                            else do
                                                putStrLn ("Directory " ++ dir ++ " already exists")
                                                theRealRunFS cpList fs
                                   else do
                                       putStrLn ("Directory " ++ (stringListToString path) ++ " doesn't exist")
                                       theRealRunFS cpList fs
                       else do
                           putStrLn ("command " ++ head commandList ++ " require more/less arguments")
                           theRealRunFS cpList fs
        "touch" -> --ok
            let content = if length commandList == 3 then (commandList !! 2) else ""
            in if length commandList == 2 || length commandList == 3
                   then let pathNfile = fst (getPath (commandList !! 1))
                            path = init pathNfile
                            file = last pathNfile
                            newFS = if head (commandList !! 1) == '/'
                                        then getFS fullFS path
                                        else Just fs
                            updatedCpList = (updateChildParentList (Directory (getName (fromJust newFS)) ((getContent (fromJust newFS)) ++ [File file content])) cpList)
                        in if isJust newFS
                               then if not (contains file (getContent (fromJust newFS)))
                                        then if getName (getParent (getName fs) updatedCpList) /= getName fs
                                                 then theRealRunFS updatedCpList (fromJust (getFS (getParent (getName fs) updatedCpList) [getName fs]))
                                                 else theRealRunFS updatedCpList (getParent (getName fs) updatedCpList)
                                        else do
                                            putStrLn ("File " ++ file ++ " already exists")
                                            theRealRunFS cpList fs
                               else do
                                   putStrLn ("Directory " ++ (stringListToString path) ++ " doesn't exist")
                                   theRealRunFS cpList fs
                   else do
                       putStrLn ("command " ++ head commandList ++ " require more/less arguments")
                       theRealRunFS cpList fs
        "rm" -> if length commandList == 2
                    then let pathNname = fst (getPath (commandList !! 1))
                             path = init pathNname
                             name = last pathNname
                             newFS = getFS fullFS path
                             updatedCpList = (updateChildParentList (Directory (getName (fromJust newFS)) (remove name (getContent (fromJust newFS)))) cpList)
                        in if isJust newFS
                               then if contains name (getContent (fromJust newFS))
                                        then if getName (getParent (getName fs) updatedCpList) /= getName fs
                                                 then theRealRunFS updatedCpList (fromJust (getFS (getParent (getName fs) updatedCpList) [getName fs]))
                                                 else theRealRunFS updatedCpList (getParent (getName fs) updatedCpList)
                                        else do
                                            putStrLn ("File/directory " ++ name ++ " doesn't exist")
                                            theRealRunFS cpList fs
                               else do
                                   putStrLn ("Directory " ++ (stringListToString path) ++ " doesn't exist")
                                   theRealRunFS cpList fs
                   else do
                       putStrLn ("command " ++ head commandList ++ " require more/less arguments")
                       theRealRunFS cpList fs
        otherwise -> do
            putStrLn ("command " ++ head commandList ++ " doesn't exist")
            theRealRunFS cpList fs

runFS :: FS -> IO () --first dir is root
runFS fs = theRealRunFS ([(getName fs, fs)] ++ getChildParentList fs) fs
