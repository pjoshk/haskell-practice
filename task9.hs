-- mapM_
-- concatMap
-- implement same interface on multiple types

main :: IO ()
main = do
  putStrLn (toEmoji Happy)
  putStrLn (toEmoji Yellow)
  putStrLn (showMoodandLight Sad Red)
  mapM_ (putStrLn.toEmoji) [Happy, Sad, Neutral]
  mapM_ (putStrLn.toEmoji) [Green, Red, Yellow]
  putStrLn (toEmoji [Happy, Sad, Neutral])

data TrafficLights = Red | Yellow | Green

data Moods = Sad | Neutral | Happy

class ToEmoji a where
  toEmoji :: a -> String

instance ToEmoji TrafficLights where
  toEmoji Red = "🔴"
  toEmoji Yellow = "🟡"
  toEmoji Green = "🟢"

instance ToEmoji a => ToEmoji [a] where
  toEmoji = concatMap toEmoji

instance ToEmoji Moods where
  toEmoji Sad = "😞"
  toEmoji Neutral = "😐"
  toEmoji Happy = "😊"

showMoodandLight :: (ToEmoji a, ToEmoji b) => a -> b -> String
showMoodandLight mood light = "Mood: " ++ toEmoji mood ++ " Light: " ++ toEmoji light


