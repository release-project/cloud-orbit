module Bench( -- sets of generators
              g
            , g1, g2, g3, g4, g5
            , g12, g13, g14, g15, g23, g24, g25, g34, g35, g45
            , g123, g124, g125, g134, g135, g145, g234, g235, g245, g345
            , g1234, g1235, g1245, g1345, g2345
            , g12345
              -- sequential benchmarks
            , seq
              -- parallel benchmarks
            , par, par_seq
              -- distributed benhcmarks
            , dist, dist_seq
            ) where

import Data.List           (lookup)
import Data.Maybe          (fromMaybe)

import Prelude      hiding (seq)
import MasterWorker        (HostInfo(..), MaybeHosts(..), orbit)

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

-----------------------------------------------------------------------------
-- benchmarks, parametrised by
-- * list of Generators
-- * size of space N > 0
-- * number of processors P > 0 (per node)
-- * list of Workers (in short node name format 'name@host')
-- sequential orbit computation
seq generators n =
    sz $ orbit (generators n) [0] (Seq (2 * n))

-- parallel orbit computation (par_seq/3 does not spawn image computation)
par generators n p =
    sz $ orbit (generators n) [0]
      (Par (JustOne (p, ((2 * n) `div` p) + 1, 0, True)))

par_seq generators n p =
    sz $ orbit (generators n) [0]
      (Par (JustOne (p, ((2 * n) `div` p) + 1, 0, False)))

-- distributed orbit computation (dist_seq/4 does not spawn image computation)
dist generators n p workers =
    sz $ orbit (generators n) [0]
      (Par (Many [(h, p, (2 * n) `div` (w * p) + 1, 1, True) | h <- workers]))
  where w = length workers

dist_seq generators n p workers =
    sz $ orbit (generators n) [0]
      (Par (Many [(h, p, (2 * n) `div` (w * p) + 1, 1, False) | h <- workers]))
  where w = length workers

sz (_, mainStats : _) =
    case "size" `lookup` mainStats of
        Nothing -> "false"
        Just s  -> "{size," ++ s ++ "}"
