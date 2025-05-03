-- Trait Bounds `=>`

main :: IO ()
main = do
  print (isEq 5 5)
  print (isEq 5 6)

isEq :: (Eq a) => a -> a -> Maybe a
isEq x y
  | x == y = Just y
  | x /= y = Nothing
