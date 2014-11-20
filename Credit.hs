--
-- orbit-int credits (for checking termination of orbit computation)
--
module Credit( -- Types
               ACredit
             , Credit
               -- Functions
             , credit
             , credit_atomic
             , debit_atomic
             , debit_atomic_nz
             , zero
             , one
             , is_zero
             , is_one
             ) where

-- An *atomic credit* is represented as a non-negative integer k;
-- it stands for the credit 1/{2^k}.
--
-- A *credit* is represented as list of non-negative integers, sorted in
-- strict descending order; it represents the sum of atomic credits
-- represented by the integers on the list, where zero credit is
-- represented by the empty list. The maximally possible credit, 1,
-- is represented by [0].

type ACredit = Int
type Credit = [ACredit]

-- credit_atomic(K, C) adds the atomic credit 1/{2^K} to the credit C.
credit_atomic :: Int -> Credit -> Credit
credit_atomic k [] = [k]
credit_atomic k (c : cs) | k > c  = k : c : cs
                         | k == c = credit_atomic (k - 1) cs
                         | otherwise = c : credit_atomic k cs

-- credit(C1, C2) returns a list representing the sum of the credit
-- represented by the lists C1 and C2.
credit :: Credit -> Credit -> Credit
credit c1 c2 = foldl (flip credit_atomic) c2 c1

-- debit_atomic(C) returns a pair {K',C'} where K' is an integer
-- representing some atomic credit and C' is a list of integers representing
-- some credit (which may be zero) such that the sum of the credits
-- represented by K' and C' equals the credit represented by C.
-- Precondition: C must represent non-zero credit.
debit_atomic :: Credit -> (ACredit, Credit)
debit_atomic [] = error "Credit [] is not valid."
debit_atomic (c : cs) = (c, cs) -- debit smallest unit of credit

-- debit_atomic_nz(C) returns a pair {K',C'} where K' is an integer
-- representing some atomic credit and C' is a list of integers representing
-- some non-zero credit such that the sum of the credits
-- represented by K' and C' equals the credit represented by C.
-- Precondition: C must represent non-zero credit.
debit_atomic_nz :: Credit -> (ACredit, Credit)
debit_atomic_nz [] = error "Credit [] is not valid."
debit_atomic_nz [c] = (c + 1, [c + 1]) -- debit half the credit
debit_atomic_nz (c : cs) = (c, cs) -- debit smallest unit of credit;
-- case only applies if Cs non-empty

-- zero/0 produces zero credit.
zero :: Credit
zero = []

-- one/0 produces credit one.
one :: Credit
one = [0]

-- is_zero/1 tests whether its argument represents zero credit.
is_zero :: Credit -> Bool
is_zero [] = True
is_zero _  = False

-- is_one/1 tests whether its argument represents maximal credit 1.
is_one :: Credit -> Bool
is_one [0] = True
is_one _   = False
