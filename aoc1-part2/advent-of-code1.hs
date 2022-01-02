import Control.Monad
import System.IO

main :: IO ()
main = do
  let sonarReadings = []
  handle <- openFile "input" ReadMode
  contents <- hGetContents handle
  let readingsAsStr = words contents
      sonarReadings = parseReadingsAsInt readingsAsStr
  let increasingMeasurements = countSums sonarReadings
  print increasingMeasurements
  hClose handle

parseReadingsAsInt :: [String] -> [Int]
parseReadingsAsInt = map read

countIncreasingMeasurements :: [Int] -> Maybe Int -> Int -> Int
countIncreasingMeasurements [] _ count = count
countIncreasingMeasurements (x : xs) prev count = case prev of
  Nothing -> countIncreasingMeasurements xs (Just x) count
  Just val ->
    if x > val
      then countIncreasingMeasurements xs (Just x) (count + 1)
      else countIncreasingMeasurements xs (Just x) count

countMeasurements :: [Int] -> Int
countMeasurements xs = countIncreasingMeasurements xs Nothing 0

countIncreasingSums :: [Int] -> Maybe Int -> Maybe Int -> Maybe Int -> Int -> Int
countIncreasingSums [] _ _ _ count = count
countIncreasingSums (x : xs) Nothing second prev count = countIncreasingSums xs (Just x) second prev count
countIncreasingSums (x : xs) first Nothing prev count = countIncreasingSums xs first (Just x) prev count
countIncreasingSums (x : xs) (Just firstValue) (Just secondValue) prev count = case prev of
  Nothing -> countIncreasingSums xs (Just secondValue) (Just x) (Just (firstValue + secondValue + x)) count
  Just prevValue ->
    if (firstValue + secondValue + x) > prevValue
      then countIncreasingSums xs (Just secondValue) (Just x) (Just (firstValue + secondValue + x)) (count + 1)
      else countIncreasingSums xs (Just secondValue) (Just x) (Just (firstValue + secondValue + x)) count

countSums :: [Int] -> Int
countSums xs = countIncreasingSums xs Nothing Nothing Nothing 0
