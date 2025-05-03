-- iterator map
-- print method
-- [expression | generator, filter]

main :: IO ()
main =
  print (doubleAll [1, 2, 3, 4])
    >> putStrLn (describe 23)
    >> putStrLn (describe 0)
    >> putStrLn (describe (-23))
    >> print sumOfEvenSqrs
    >> putStrLn "What's your name?"
    >> getLine
    >>= \name ->
      print (firstLetter name)

doubleAll :: [Int] -> [Int]
doubleAll xs = [x * 2 | x <- xs]

describe :: Int -> String
describe n
  | n > 0 = "Positive"
  | n < 0 = "Negative"
  | n == 0 = "Zero"

sumOfEvenSqrs :: Int
sumOfEvenSqrs = sum [x * x | x <- [1 .. 100], even x]

firstLetter :: String -> Char
firstLetter "" = '\0'
firstLetter (x : _) = x
