-- input/output
-- parsing string to int
-- case statements

main :: IO ()

main = do
  putStrLn "What is your name?"
  input <- getLine
  putStrLn ("Hi" ++ input)
  putStrLn "What is your favorite number?"
  input <- getLine
  case reads input of
    [(n, "")] -> putStrLn("you got a " ++ show(n * 2))
    _ -> putStrLn "That's not a valid number"

  

