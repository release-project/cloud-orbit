import           Control.Concurrent.MVar               (MVar, putMVar,
                                                        newEmptyMVar, takeMVar)
import           Control.Distributed.Process
import           Control.Distributed.Process.Node
import qualified Network.Transport as NT               (Transport)
import           Network.Transport.TCP
import           Prelude                        hiding (seq)
import           Test.Framework                        (Test, testGroup,
                                                        defaultMain)
import           Test.Framework.Providers.HUnit        (testCase)
import           Test.HUnit                            (Assertion)
import           Test.HUnit.Base                       (assertBool)

import           Bench                                 (seq)
import           MasterWorker                          (__remoteTable)
import           Utils

-- Sequential Tests

testSeqShort :: TestResult String -> Process ()
testSeqShort result = do
    x <- seq gg13 11
    stash result x

testSeqIntermediate :: TestResult String -> Process ()
testSeqIntermediate result = do
    x <- seq gg124 157
    stash result x

testSeqLong :: TestResult String -> Process ()
testSeqLong result = do
    x <- seq gg1245 157
    stash result x

-- Batch the tests

tests :: LocalNode  -> [Test]
tests localNode = [
    testGroup "Sequential Tests" [
          testCase "testSeqShort"
            (delayedAssertion "short" localNode "{size,10}" testSeqShort)
        , testCase "testSeqIntermediate"
            (delayedAssertion "intermediate" localNode "{size,133}" testSeqIntermediate)
        , testCase "testSeqLong"
            (delayedAssertion "long" localNode "{size,134}" testSeqLong)
      ]
  ]

-- Run the tests

orbitTests :: NT.Transport -> IO [Test]
orbitTests transport = do
    localNode <- newLocalNode transport rtable
    let testData = tests localNode
    return testData
  where rtable :: RemoteTable
        rtable = MasterWorker.__remoteTable initRemoteTable

main :: IO ()
main = testMain $ orbitTests

-- Auxiliary functions
-------------------------------------------------------------------

-- | A mutable cell containing a test result.
type TestResult a = MVar a

-- | Stashes a value in our 'TestResult' using @putMVar@
stash :: TestResult a -> a -> Process ()
stash mvar x = liftIO $ putMVar mvar x

-- | Run the supplied @testProc@ using an @MVar@ to collect and assert
-- against its result. Uses the supplied @note@ if the assertion fails.
delayedAssertion :: (Eq a) => String -> LocalNode -> a ->
                    (TestResult a -> Process ()) -> Assertion
delayedAssertion note localNode expected testProc = do
    result <- newEmptyMVar
    _ <- forkProcess localNode $ testProc result
    assertComplete note result expected

-- | Takes the value of @mv@ (using @takeMVar@) and asserts that it matches @a@
assertComplete :: (Eq a) => String -> MVar a -> a -> IO ()
assertComplete msg mv a = do
    b <- takeMVar mv
    assertBool msg (a == b)

-- | Given a @builder@ function, make and run a test suite on a single transport
testMain :: (NT.Transport -> IO [Test]) -> IO ()
testMain builder = do
    Right (transport, _) <-
      createTransportExposeInternals "127.0.0.1" "10501" defaultTCPParameters
    testData <- builder transport
    defaultMain testData
