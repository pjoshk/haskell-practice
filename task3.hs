-- Cons a rest // constructor of list

main :: IO ()
main = do
  print list
  print (mySum list)
  print (myMap myDouble list)

data MyList a = Empty | Cons a (MyList a) deriving (Show)

list :: MyList Int
list = Cons 2 (Cons 3 (Cons 4 Empty))

myLength :: MyList a -> Int
myLength Empty = 0
myLength (Cons _ xs) = 1 + myLength xs

mySum :: MyList Int -> Int
mySum Empty = 0
mySum (Cons a xs) = a + mySum xs

myMap :: (a -> b) -> MyList a -> MyList b
myMap _ Empty = Empty
myMap f (Cons x xs) = Cons (f x) (myMap f xs)

myDouble :: Int -> Int
myDouble a = a * 2
