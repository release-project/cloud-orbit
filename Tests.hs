module Main where

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

import           Bench                                 (seq, par, dist, sz)
import           MasterWorker                          (__remoteTable)
import           Utils

-- Sequential Tests

testSeqShort :: TestResult String -> Process ()
testSeqShort result = do
    x <- seq gg13 11
    stash result (getRes x)

testSeqIntermediate :: TestResult String -> Process ()
testSeqIntermediate result = do
    x <- seq gg124 157
    stash result (getRes x)

testSeqLong :: TestResult String -> Process ()
testSeqLong result = do
    x <- seq gg1245 157
    stash result (getRes x)

-- Parallel Tests

testParShort :: TestResult String -> Process ()
testParShort result = do
    x <- par True gg13 11 2
    stash result (getRes x)

testParIntermediate :: TestResult String -> Process ()
testParIntermediate result = do
    x <- par True gg124 157 2
    stash result (getRes x)

testParLong :: TestResult String -> Process ()
testParLong result = do
    x <- par True gg1245 157 2
    stash result (getRes x)

testParSeqShort :: TestResult String -> Process ()
testParSeqShort result = do
    x <- par False gg13 11 2
    stash result (getRes x)

testParSeqIntermediate :: TestResult String -> Process ()
testParSeqIntermediate result = do
    x <- par False gg124 157 2
    stash result (getRes x)

testParSeqLong :: TestResult String -> Process ()
testParSeqLong result = do
    x <- par False gg1245 157 2
    stash result (getRes x)

-- Distributed Tests

testDistShort :: [NodeId] -> TestResult String -> Process ()
testDistShort nodes result = do
    x <- dist True gg13 11 2 nodes
    stash result (getRes x)

testDistIntermediate :: [NodeId] -> TestResult String -> Process ()
testDistIntermediate nodes result = do
    x <- dist True gg124 157 2 nodes
    stash result (getRes x)

testDistLong :: [NodeId] -> TestResult String -> Process ()
testDistLong nodes result = do
    x <- dist True gg1245 157 2 nodes
    stash result (getRes x)

testDistSeqShort :: [NodeId] -> TestResult String -> Process ()
testDistSeqShort nodes result = do
    x <- dist False gg13 11 2 nodes
    stash result (getRes x)

testDistSeqIntermediate :: [NodeId] -> TestResult String -> Process ()
testDistSeqIntermediate nodes result = do
    x <- dist False gg124 157 2 nodes
    stash result (getRes x)

testDistSeqLong :: [NodeId] -> TestResult String -> Process ()
testDistSeqLong nodes result = do
    x <- dist False gg1245 157 2 nodes
    stash result (getRes x)

-- Batch the tests

tests :: [LocalNode] -> [Test]
tests [] = []
tests (localNode : localNodes) = [
      testGroup "Sequential Tests" [
            testCase "testSeqShort"
              (delayedAssertion "short" localNode "{size,10}" testSeqShort)
          , testCase "testSeqIntermediate"
              (delayedAssertion "intermediate" localNode "{size,133}" testSeqIntermediate)
          , testCase "testSeqLong"
              (delayedAssertion "long" localNode "{size,134}" testSeqLong)
        ]
    , testGroup "Parallel Tests" [
            testCase "testParSeqShort"
              (delayedAssertion "short" localNode "{size,10}" testParSeqShort)
          , testCase "testParSeqIntermediate"
              (delayedAssertion "intermediate" localNode "{size,133}" testParSeqIntermediate)
          , testCase "testParSeqLong"
              (delayedAssertion "long" localNode "{size,134}" testParSeqLong)
          , testCase "testParShort"
              (delayedAssertion "short" localNode "{size,10}" testParShort)
          , testCase "testParIntermediate"
              (delayedAssertion "intermediate" localNode "{size,133}" testParIntermediate)
          , testCase "testParLong"
              (delayedAssertion "long" localNode "{size,134}" testParLong)
        ]
    , testGroup "Distributed Tests" [
            testCase "testDistSeqShort"
              (delayedAssertion "short" localNode "{size,10}" $
                testDistSeqShort (map localNodeId localNodes))
          , testCase "testDistSeqIntermediate"
              (delayedAssertion "intermediate" localNode "{size,133}" $
                testDistSeqIntermediate (map localNodeId localNodes))
          , testCase "testDistSeqLong"
              (delayedAssertion "long" localNode "{size,134}" $
                testDistSeqLong (map localNodeId localNodes))
          , testCase "testDistShort"
              (delayedAssertion "short" localNode "{size,10}" $
                testDistShort (map localNodeId localNodes))
          , testCase "testDistIntermediate"
              (delayedAssertion "intermediate" localNode "{size,133}" $
                testDistIntermediate (map localNodeId localNodes))
          , testCase "testDistLong"
              (delayedAssertion "long" localNode "{size,134}" $
                testDistLong (map localNodeId localNodes))
       ]
  ]

-- Run the tests

orbitTests :: NT.Transport -> IO [Test]
orbitTests transport = do
    localNode  <- newLocalNode transport rtable
    localNode2 <- newLocalNode transport rtable
    localNode3 <- newLocalNode transport rtable
    let testData = tests [localNode, localNode2, localNode3]
    return testData
  where rtable :: RemoteTable
        rtable = MasterWorker.__remoteTable initRemoteTable

main :: IO ()
main = testMain $ orbitTests

-- Auxiliary functions
-------------------------------------------------------------------

-- | Gets the size from MasterStats.
getRes = Result -> String
getRes = sz . snd

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
