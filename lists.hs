main :: IO ()
main = do
  print list
  print (push 8 list)
  print (insert 23 2 list)
  print (update 56 4 list)
  print (remove 1 list)
  print (pop list)
  print (list !! 3)

list = [1, 2, 3, 4, 5]

-- CRUD
push :: Int -> [Int] -> [Int]
push a arr = arr ++ [a]

insert :: Int -> Int -> [Int] -> [Int]
insert a i arr = take i arr ++ [a] ++ drop i arr

len :: [Int] -> Int
len xs = sum [1 | x <- xs]

update :: Int -> Int -> [Int] -> [Int]
update a i arr = take i arr ++ [a] ++ drop (i + 1) arr

remove :: Int -> [Int] -> [Int]
remove i arr = take i arr ++ drop (i + 1) arr

pop :: [Int] -> [Int]
pop arr = take (len arr - 1) arr
