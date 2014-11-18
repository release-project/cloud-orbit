import Test.HUnit

import Bench (seq, g13, g124, g1245)

test1 = TestCase (assertEqual "g13 11" "{size,10}" (Bench.seq g13 11))
test2 = TestCase (assertEqual "g124 157" "{size,133}" (Bench.seq g124 157))
test3 = TestCase (assertEqual "g1245 157" "{size,134}" (Bench.seq g1245 157))

tests = TestList [ TestLabel "seq short" test1
                 , TestLabel "seq medium" test2
                 , TestLabel "seq long" test3
                 ]
