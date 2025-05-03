import Text.Read (readMaybe)

main :: IO ()
main = loop emotionList

loop :: [Moods] -> IO ()
loop eList = putStrLn "Enter your mood (Sad | Neutral | Happy), or 'quit' to exit:" >>
  getLine >>= \input ->
  case handleInput input of
    Left m -> loop (eList ++ [m])
    Right Quit -> putStrLn ("You logged " ++ show (length eList) ++ " moods.") >>
      putStrLn ("Emojis: " ++ printEmojis eList) >>
      putStrLn ("Average Mood Score: " ++ show (avgMoodScore eList)) >>
      putStrLn ("Trend: " ++ describeTrend eList)
    Right ParseError -> putStrLn "Parse Error" >> loop eList

emotionList = []

data Moods = Sad | Neutral | Happy deriving (Eq, Ord, Read, Show)

data Err = ParseError | Quit deriving (Read, Eq, Show)

moodScore :: Moods -> Int
moodScore Sad = -5
moodScore Neutral = 0
moodScore Happy = 5

avgMoodScore :: [Moods] -> Float
avgMoodScore moods = fromIntegral (sum [moodScore m | m <- moods]) / fromIntegral (length moods)

describeTrend :: [Moods] -> String
describeTrend moods
  | sum [moodScore m | m <- moods] > 0 = "Improving"
  | sum [moodScore m | m <- moods] < 0 = "Regressing"
  | otherwise = "Neutral"


handleInput :: String -> Either Moods Err
handleInput s
  | s == "quit" = Right Quit
  | otherwise = case readMaybe s of
    Just mood -> Left mood
    Nothing -> Right ParseError  

toEmoji :: Moods -> String
toEmoji m = case m of
  Sad -> "😔"
  Neutral -> "😑"
  Happy -> "😊"

printEmojis :: [Moods] -> String
printEmojis moods = foldr (++) " " [toEmoji x | x <- moods]
  





