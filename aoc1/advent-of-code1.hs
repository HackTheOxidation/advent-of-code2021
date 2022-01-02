import System.IO
import Control.Monad

main :: IO ()
main = do
        let sonarReadings = []
        handle <- openFile "input" ReadMode
        contents <- hGetContents handle
        let readingsAsStr = words contents
            sonarReadings = parseReadingsAsInt readingsAsStr
        let increasingMeasurements = countMeasurements sonarReadings
        print increasingMeasurements
        hClose handle


parseReadingsAsInt :: [String] -> [Int]
parseReadingsAsInt = map read

countIncreasingMeasurements :: [Int] -> Maybe Int -> Int -> Int
countIncreasingMeasurements [] _ count = count
countIncreasingMeasurements (x:xs) prev count = case prev of
                                                  Nothing -> countIncreasingMeasurements xs (Just x) count
                                                  Just val -> if x > val then
                                                                         countIncreasingMeasurements xs (Just x) (count + 1)
                                                                         else
                                                                         countIncreasingMeasurements xs (Just x) count

countMeasurements :: [Int] -> Int
countMeasurements xs = countIncreasingMeasurements xs Nothing 0
