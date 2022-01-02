import Basement.IntegralConv
import Numeric.Extra
import System.Environment
import System.IO

parseStringsToBinary :: [String] -> [[Int]]
parseStringsToBinary = map parseCharListToBinary

parseCharListToBinary :: [Char] -> [Int]
parseCharListToBinary = map (\c -> read [c])

countOccurrencesOfOne :: [Int] -> [Int] -> [Int]
countOccurrencesOfOne = zipWith (+)

countOccurrences :: [[Int]] -> [Int] -> [Int]
countOccurrences [] occurrences = occurrences
countOccurrences (x : xs) occurrences = foldl (flip countOccurrencesOfOne) occurrences xs

ratiosToMostCommon :: [Int] -> Int -> [Int]
ratiosToMostCommon [] _ = []
ratiosToMostCommon (ratio : ratios) len =
  if ratio >= div len 2
    then 1 : ratiosToMostCommon ratios len
    else 0 : ratiosToMostCommon ratios len

mostCommonToLeastCommon :: [Int] -> [Int]
mostCommonToLeastCommon [] = []
mostCommonToLeastCommon (x : xs) =
  if x == 1
    then 0 : mostCommonToLeastCommon xs
    else 1 : mostCommonToLeastCommon xs

binaryToDecimal :: [Int] -> Int -> Int
binaryToDecimal [] _ = 0
binaryToDecimal (x : xs) exp = x * exp + binaryToDecimal xs exp * 2

binToDec :: [Int] -> Int
binToDec xs = binaryToDecimal (reverse xs) 1

calculatePowerConsumption :: [Int] -> [Int] -> Int
calculatePowerConsumption gamma epsilon = binToDec gamma * binToDec epsilon

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  let linesInFile = lines content
  let binaryNumbers = parseStringsToBinary linesInFile
  let len = length (head binaryNumbers)
  let gamma = ratiosToMostCommon (countOccurrences binaryNumbers (replicate len 0)) (length binaryNumbers)
  print gamma
  let epsilon = mostCommonToLeastCommon gamma
  print epsilon
  let powerConsumption = calculatePowerConsumption gamma epsilon
  print powerConsumption
