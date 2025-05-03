main :: IO ()
main = putStrLn (excite "Wow")

excite :: String -> String
excite phrase = phrase ++ "!!!"
