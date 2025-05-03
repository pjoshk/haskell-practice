-- lazy eval
-- real fast

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

fibn :: Int -> Integer
fibn n = last (take (n + 1) fib)
