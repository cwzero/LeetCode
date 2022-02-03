{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import qualified Data.Map as Dm
import Data.List

type Bucket = [Int]
type BucketMap = Dm.Map Char Bucket
type CountMap = Dm.Map Char Int

main = do
    print (drop 3 "abcde")
    print (take 3 "abcde")

--main = print (analyzeAnagrams "aaaaa" "aaa")

between :: Int -> Int -> (Int -> Bool)
between start end test = test >= start && test <= end

filterMap :: Int -> Int -> (BucketMap -> BucketMap)
filterMap start end = Dm.map (filter (between start end))

analyze :: String -> BucketMap
analyze s = Dm.fromList $ map (\letter -> (letter, elemIndices letter s)) (nub $ sort s)

countMap :: BucketMap -> CountMap
countMap = Dm.map length

compareMap :: CountMap -> (CountMap -> Bool)
compareMap mapA mapB =
    all (\letter -> Dm.lookup letter mapA == Dm.lookup letter mapB) (Dm.keys mapA)

compareBucket :: Bucket -> (Bucket -> Bool)
compareBucket bA bB = length bA == length bB

analyzeAnagrams :: String -> String -> [Int]
analyzeAnagrams source target = searchAnagrams source target [] 0

searchAnagram :: String -> String -> Int -> Bool
searchAnagram source target i = compareMap (countMap $ analyze (drop i source)) (countMap $ analyze target)

searchAnagrams :: String -> String -> [Int] -> Int -> [Int]
searchAnagrams source target x i | i >= length source - length target = x
searchAnagrams source target x i | i < length source - length target =
    if searchAnagram source target i
        then x ++ searchAnagrams source target [i] (i + 1)
    else x ++ searchAnagrams source target [] (i + 1)