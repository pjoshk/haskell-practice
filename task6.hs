-- if then else
-- Either

main :: IO ()
main = case safeDiv 12 0 of
  Left s -> putStrLn s
  Right x -> putStrLn ("Result: " ++ show x)

safeDiv :: Int -> Int -> Either String Int
safeDiv x y = if y == 0 then Left "Cannot divide by Zero" else Right (x `div` y)
