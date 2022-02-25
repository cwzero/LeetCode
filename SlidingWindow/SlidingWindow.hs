import Data.List

main :: IO ()
main = print $ slidingWindow [1,3,-1,-3,5,3,6,7] 3

slidingWindow :: [Int] -> Int -> [Int]
slidingWindow list k = map maximum (mapWindow list k)
        where lastIndex = length list - k

mapWindow :: [Int] -> Int -> [[Int]]
mapWindow list k = map (window list k) [0..lastIndex]
        where lastIndex = length list - k

window :: [Int] -> Int -> Int -> [Int]
window list k i = take k $ drop i list
