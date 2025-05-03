-- monads
-- desugar do
-- chaining monads

-- main = do
--   putStrLn "Enter your name:"
--   name <- getLine
--   putStrLn ("Hello, " ++ name)

main :: IO ()
main = putStrLn "Enter your name" >>
  getLine >>= \name ->
  putStrLn ("Hello, " ++ name)
