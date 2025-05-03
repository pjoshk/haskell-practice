-- Interfaces are called TypeClasses in Haskell
-- Describable is an interface

main :: IO ()
main = putStrLn (describe Red)

class Describable a where
  describe :: a -> String

data TrafficLight = Red | Yellow | Green

instance Describable TrafficLight where
  describe Red = "Stop!"
  describe Yellow = "Wait"
  describe Green = "Go!"
 
