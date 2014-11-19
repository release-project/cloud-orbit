import Prelude                        hiding (seq)
import Test.HUnit
import Test.Framework                        (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Bench                                 (seq)
import Utils                                 (g13, g124, g1245)

main = defaultMain tests

tests = [ testGroup "Sequential tests" [
                testCase "short" test1,
                testCase "intermediate" test2,
                testCase "long" test3
            ]
        ]

test1 = assertEqual "g13 11" "{size,10}" (seq g13 11)
test2 = assertEqual "g124 157" "{size,133}" (seq g124 157)
test3 = assertEqual "g1245 157" "{size,134}" (seq g1245 157)
