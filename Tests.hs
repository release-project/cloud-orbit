import Test.HUnit
import Test.Framework                 (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Bench (seq, g13, g124, g1245)

main = defaultMain tests

tests = [ testGroup "Sequential tests" [
                testCase "short" test1,
                testCase "intermediate" test2,
                testCase "long" test3
            ]
        ]

test1 = assertEqual "g13 11" "{size,10}" (Bench.seq g13 11)
test2 = assertEqual "g124 157" "{size,133}" (Bench.seq g124 157)
test3 = assertEqual "g1245 157" "{size,134}" (Bench.seq g1245 157)
