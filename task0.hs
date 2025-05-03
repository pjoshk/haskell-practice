main :: IO ()

main = do
  putStrLn "What is your name?"
  input <- getLine
  putStrLn ("Hi" ++ input)
  putStrLn "What is your favorite number?"
  input <- getLine
  let num = read input :: Int
  putStrLn ("Double: " ++ show(num * 2))

  

