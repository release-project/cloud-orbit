{-# LANGUAGE DeriveDataTypeable #-}
module Utils where

import Data.Binary
import Data.Typeable

-- Trying to serialize ParConf closures...
newtype GenClos = GenClos (String, Int, [Generator])
    deriving (Typeable)

instance Show GenClos where
    showsPrec _ (GenClos (name, _, _)) = (name ++)

instance Binary GenClos where
    put (GenClos (name, n, _)) = put (name, n)
    get = get >>= \(name, n) -> return $ GenClos (name, n, dispatcher name n)

type Generator = Vertex -> Vertex
type Vertex = Int

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
f1, f2, f3, f4, f5 :: Int -> Int -> Int
f1 n x = r n $ (fib (p3 1 0 (r 16 x))) + p3 1 0 x
f2 n x = r n $ (fib (p3 1 5 (r 16 x))) + p4 2 5 (-1) x
f3 n x = r n $ (fib (p3 1 10 (r 16 x))) + p5 (-1) 0 8 0 x
f4 n x = r n $ (fib (p3 8 3 (s5 0 49 98 100 (r 100 x)))) + p2 (-1) x
f5 n x = r n $ (fib (p3 10 0 (s5 0 900 999 1000 (r 1000 x)))) + p2 1 x

-- sets (= lists) of generators
g :: Vertex -> [Generator]
g _ = []

gg :: Vertex -> GenClos
gg n = GenClos ("g", n, (g n))

g1, g2, g3, g4, g5 :: Vertex -> [Generator]
g1 n = [f1 n]
g2 n = [f2 n]
g3 n = [f3 n]
g4 n = [f4 n]
g5 n = [f5 n]

gg1, gg2, gg3, gg4, gg5 :: Vertex -> GenClos
gg1 n = GenClos ("g1", n, (g1 n))
gg2 n = GenClos ("g2", n, (g2 n))
gg3 n = GenClos ("g3", n, (g3 n))
gg4 n = GenClos ("g4", n, (g4 n))
gg5 n = GenClos ("g5", n, (g5 n))

g12, g13, g14, g15, g23, g24, g25, g34, g35, g45 :: Vertex -> [Generator]
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

gg12, gg13, gg14, gg15, gg23, gg24, gg25, gg34, gg35, gg45 :: Vertex -> GenClos
gg12 n = GenClos ("g12", n, (g12 n))
gg13 n = GenClos ("g13", n, (g13 n))
gg14 n = GenClos ("g14", n, (g14 n))
gg15 n = GenClos ("g15", n, (g15 n))
gg23 n = GenClos ("g23", n, (g23 n))
gg24 n = GenClos ("g24", n, (g24 n))
gg25 n = GenClos ("g25", n, (g25 n))
gg34 n = GenClos ("g34", n, (g34 n))
gg35 n = GenClos ("g35", n, (g35 n))
gg45 n = GenClos ("g45", n, (g45 n))

g123, g124, g125, g134, g135, g145, g234, g235, g245, g345
  :: Vertex -> [Generator]
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

gg123, gg124, gg125, gg134, gg135, gg145, gg234, gg235, gg245, gg345
  :: Vertex -> GenClos
gg123 n = GenClos ("g123", n, (g123 n))
gg124 n = GenClos ("g124", n, (g124 n))
gg125 n = GenClos ("g125", n, (g125 n))
gg134 n = GenClos ("g134", n, (g134 n))
gg135 n = GenClos ("g135", n, (g135 n))
gg145 n = GenClos ("g145", n, (g145 n))
gg234 n = GenClos ("g234", n, (g234 n))
gg235 n = GenClos ("g235", n, (g235 n))
gg245 n = GenClos ("g245", n, (g245 n))
gg345 n = GenClos ("g345", n, (g345 n))

g1234, g1235, g1245, g1345, g2345 :: Vertex -> [Generator]
g1234 n = g123 n ++ g4 n
g1235 n = g123 n ++ g5 n
g1245 n = g124 n ++ g5 n
g1345 n = g134 n ++ g5 n
g2345 n = g234 n ++ g5 n

gg1234, gg1235, gg1245, gg1345, gg2345 :: Vertex -> GenClos
gg1234 n = GenClos ("g1234", n, (g1234 n))
gg1235 n = GenClos ("g1235", n, (g1235 n))
gg1245 n = GenClos ("g1245", n, (g1245 n))
gg1345 n = GenClos ("g1345", n, (g1345 n))
gg2345 n = GenClos ("g2345", n, (g2345 n))

g12345 :: Vertex -> [Generator]
g12345 n = g1234 n ++ g5 n

gg12345 :: Vertex -> GenClos
gg12345 n = GenClos ("g12345", n, (g12345 n))

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
