-- loop
-- Multiple derive
-- safe parse
-- default values

main :: IO ()
main = loop

loop :: IO ()

loop = do
  putStrLn "How's your mood now? [Sad | Happy | Neutral]"
  input <- getLine
  let moodA = safeMood input
  putStrLn "How's your mood now? [Sad | Happy | Neutral]"
  input <- getLine
  let moodB = safeMood input
  putStrLn (compareMoods moodA moodB)
  loop

data Mood = Sad | Neutral | Happy deriving (Eq, Ord, Show, Read)

compareMoods :: Mood -> Mood -> String
compareMoods a b
  | a > b = "Mood Worsened"
  | a < b = "Mood Improved"
  | otherwise = "No Change"

safeMood :: String -> Mood
safeMood s = case reads s of
  [(m, "")] -> m
  _ -> Neutral
