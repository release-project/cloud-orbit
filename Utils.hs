module Utils where

-----------------------------------------------------------------------------
-- generators

-- Fibonacci numbers
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- mixing polynomials (up to degree 3)
p2 :: Int -> Int -> Int
p2 a0 _ = a0

p3 :: Int -> Int -> Int -> Int
p3 a1 a0 n = a1 * n + p2 a0 n

p4 :: Int -> Int -> Int -> Int -> Int
p4 a2 a1 a0 n = a2 * n * n + p3 a1 a0 n

p5 :: Int -> Int -> Int -> Int -> Int -> Int
p5 a3 a2 a1 a0 n = a3 * n * n * n + p4 a2 a1 a0 n

-- step functions (up to 4 steps)
s2 :: Int -> Int -> Int
s2 b0 n | n < b0    = 0
        | otherwise = 1

s3 :: Int -> Int -> Int -> Int
s3 b0 b1 n | n < b0    = 0
           | otherwise = 1 + s2 b1 n

s4 :: Int -> Int -> Int -> Int -> Int
s4 b0 b1 b2 n | n < b0    = 0
              | otherwise = 1 + s3 b1 b2 n

s5 :: Int -> Int -> Int -> Int -> Int -> Int
s5 b0 b1 b2 b3 n | n < b0    = 0
                 | otherwise = 1 + s4 b1 b2 b3 n

-- remainder function (range 0..R - 1)
r :: Int -> Int -> Int
r r0 n = (abs n) `rem` r0

-- generators based on Fibonacci numbers;
-- functions f1(N,_),...,f5(N,_) produce numbers in the range 0 .. N-1;
-- computationally f1 = fib(0..15),
-- f2 = fib(5..20),
-- f3 = fib(10..25),
-- f4 = fib(11,19,27), bias 49- to 11, 49- to 19, 2- to 27
-- f5 = fib(10,20,30), bias 90- to 10, 9.9- to 20, 0.1- to 30
f1 n x = r n $ (fib (p3 1 0 (r 16 x))) + p3 1 0 x
f2 n x = r n $ (fib (p3 1 5 (r 16 x))) + p4 2 5 (-1) x
f3 n x = r n $ (fib (p3 1 10 (r 16 x))) + p5 (-1) 0 8 0 x
f4 n x = r n $ (fib (p3 8 3 (s5 0 49 98 100 (r 100 x)))) + p2 (-1) x
f5 n x = r n $ (fib (p3 10 0 (s5 0 900 999 1000 (r 1000 x)))) + p2 1 x

-- sets (= lists) of generators
g _ = []

g1 n = [f1 n]
g2 n = [f2 n]
g3 n = [f3 n]
g4 n = [f4 n]
g5 n = [f5 n]

g12 n = g1 n ++ g2 n
g13 n = g1 n ++ g3 n
g14 n = g1 n ++ g4 n
g15 n = g1 n ++ g5 n
g23 n = g2 n ++ g3 n
g24 n = g2 n ++ g4 n
g25 n = g2 n ++ g5 n
g34 n = g3 n ++ g4 n
g35 n = g3 n ++ g5 n
g45 n = g4 n ++ g5 n

g123 n = g12 n ++ g3 n
g124 n = g12 n ++ g4 n
g125 n = g12 n ++ g5 n
g134 n = g13 n ++ g4 n
g135 n = g13 n ++ g5 n
g145 n = g14 n ++ g5 n
g234 n = g23 n ++ g4 n
g235 n = g23 n ++ g5 n
g245 n = g24 n ++ g5 n
g345 n = g34 n ++ g5 n

g1234 n = g123 n ++ g4 n
g1235 n = g123 n ++ g5 n
g1245 n = g124 n ++ g5 n
g1345 n = g134 n ++ g5 n
g2345 n = g234 n ++ g5 n

g12345 n = g1234 n ++ g5 n

dispatcher :: String -> Int -> [Int -> Int]
dispatcher "g" = g
dispatcher "g1" = g1
dispatcher "g2" = g2
dispatcher "g3" = g3
dispatcher "g4" = g4
dispatcher "g5" = g5
dispatcher "g12" = g12
dispatcher "g13" = g13
dispatcher "g14" = g14
dispatcher "g15" = g15
dispatcher "g23" = g23
dispatcher "g24" = g24
dispatcher "g25" = g25
dispatcher "g34" = g34
dispatcher "g35" = g35
dispatcher "g45" = g45
dispatcher "g123" = g123
dispatcher "g124" = g124
dispatcher "g125" = g125
dispatcher "g134" = g134
dispatcher "g135" = g135
dispatcher "g145" = g145
dispatcher "g234" = g234
dispatcher "g235" = g235
dispatcher "g245" = g245
dispatcher "g345" = g345
dispatcher "g1234" = g1234
dispatcher "g1235" = g1235
dispatcher "g1245" = g1245
dispatcher "g1345" = g1345
dispatcher "g2345" = g2345
dispatcher "g12345" = g12345

-- current wall clock time (in milliseconds since start of RTS)
-- FIXME: get current wall clock time (maybe with getCPUTime of System.CPUTime)
now :: Int
now = 42
