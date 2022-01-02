import System.Environment
import System.IO

data SubmarineCommand = Forward Int | Up Int | Down Int

parseSubmarineCommands :: [(String, Int)] -> [SubmarineCommand]
parseSubmarineCommands [] = []
parseSubmarineCommands (pair : pairs)
  | fst pair == "forward" = Forward (snd pair) : parseSubmarineCommands pairs
  | fst pair == "up" = Up (snd pair) : parseSubmarineCommands pairs
  | fst pair == "down" = Down (snd pair) : parseSubmarineCommands pairs

splitToPairs :: [String] -> [(String, Int)]
splitToPairs [] = []
splitToPairs (str : strs) = pair : splitToPairs strs
  where
    pair = (head listOfWords, read (listOfWords !! 1))
      where
        listOfWords = words str

accumulateSubmarineCommands :: [SubmarineCommand] -> (Int, Int) -> (Int, Int)
accumulateSubmarineCommands [] coordinates = coordinates
accumulateSubmarineCommands (command : commands) (hori, dep) = case command of
  Forward val -> accumulateSubmarineCommands commands (hori + val, dep)
  Down val -> accumulateSubmarineCommands commands (hori, dep + val)
  Up val -> accumulateSubmarineCommands commands (hori, dep - val)

reduceCoordinate :: (Int, Int) -> Int
reduceCoordinate (hori, dep) = hori * dep

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (head args)
  let linesOfFile = lines content
  let result = reduceCoordinate (accumulateSubmarineCommands (parseSubmarineCommands (splitToPairs linesOfFile)) (0, 0))
  print result
