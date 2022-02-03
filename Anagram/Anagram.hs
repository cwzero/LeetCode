import qualified Data.Map as Dm
import Data.List
import Data.Maybe
import System.Environment

type Bucket = [Int]
type BucketMap = Dm.Map Char Bucket

main = do
    args <- getArgs
    let source = (head args)
    let target = (head . tail $ args)
    print $ analyzeAnagrams source target

analyzeAnagrams :: String -> String -> [Int]
analyzeAnagrams source target = searchAnagrams source target [] 0

searchAnagrams :: String -> String -> [Int] -> Int -> [Int]
searchAnagrams source target x i | i > length source - length target = x
searchAnagrams source target x i | i <= length source - length target =
    searchAnagrams source target pre (i + 1)
    where pre = x ++ searchAnagram source target i

searchAnagram :: String -> String -> Int -> [Int]
searchAnagram source target i = 
    if compareMap (filterAnalyze start end source) (analyze target)
        then [i]
        else []
    where start = i
          end = (i + (length target))

filterAnalyze :: Int -> Int -> (String -> BucketMap)
filterAnalyze start end = filterMap start end . analyze

analyze :: String -> BucketMap
analyze s = Dm.fromList $ map (\letter -> (letter, elemIndices letter s)) (nub $ sort s)

filterMap :: Int -> Int -> (BucketMap -> BucketMap)
filterMap start end = Dm.map $ filter $ between start end

compareMap :: BucketMap -> (BucketMap -> Bool)
compareMap source target =
    all (compareLetter source target) (getKeys [source, target])

compareLetter :: BucketMap -> BucketMap -> Char -> Bool
compareLetter source target letter =
    if (isNothing bA) || (isNothing bB)
        then False
    else compareBucket (fromMaybe [] bA) (fromMaybe [] bB)
    where bA = (Dm.lookup letter source)
          bB = (Dm.lookup letter target)

compareBucket :: Bucket -> (Bucket -> Bool)
compareBucket bA bB = length bA == length bB

getKeys :: [BucketMap] -> [Char]
getKeys maps = nub $ sort $ concat $ map Dm.keys maps

between :: Int -> Int -> (Int -> Bool)
between start end test = test >= start && test < end