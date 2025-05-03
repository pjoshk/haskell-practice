-- enums

main :: IO ()
main = do
  print (area (Circle 3))
  print (area (Rectangle 3 4))
  putStrLn (lightAction Red)

data Shape
  = Circle Float
  | Rectangle Float Float
  deriving (Show)

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

data TrafficLight = Red | Yellow | Green

lightAction :: TrafficLight -> String
lightAction Red = "Stop!"
lightAction Yellow = "Ready"
lightAction Green = "Go!"
