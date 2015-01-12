module Bench( -- sequential benchmarks
              seq
              -- parallel benchmarks
            , par
              -- distributed benhcmarks
            , dist
              -- miscellaneous
            , main
            , getAnswer
            ) where

import           Control.Distributed.Process
import qualified Control.Distributed.Process.Backend.SimpleLocalnet as SLN
import           Control.Distributed.Process.Node
import           Prelude                                                    hiding (seq)
import           Network.Transport.TCP
import           System.Environment                                                (getArgs)

import           MasterWorker
import           Utils

type Result = ([Vertex], [MasterStats])
--type Result = String

-- | Gets the result of the calculation
result :: ([Vertex], [MasterStats]) -> Result
result = id
--result = sz . snd

-- | Gets the size (as a string) from the result
getAnswer :: Result -> String
getAnswer = sz . snd
--getAnswer = id

-----------------------------------------------------------------------------
-- benchmarks, parametrised by
-- * list of Generators
-- * size of space N > 0
-- * number of processors P > 0 (per node)
-- * list of Workers (in short node name format 'name@host')
-- sequential orbit computation
seq :: (Vertex -> GenClos) -> Vertex -> Process Result
seq generators n =
    orbit (generators n) [0] (Seq (2 * n))
      >>= return . result

-- parallel orbit computation (w/ False does not spawn image computation)
par :: Bool -> (Vertex -> GenClos) -> Vertex -> Int -> Process Result
par iwp generators n p =
     orbit (generators n) [0]
       (Par (JustOne (p, ((2 * n) `div` p) + 1, 0, iwp)))
       >>= return . result

-- distributed orbit computation (w/ False does not spawn image computation)
dist :: Bool -> (Vertex -> GenClos) -> Vertex -> Int -> [NodeId] -> Process Result
dist iwp generators n p workers =
    orbit (generators n) [0]
      (Par (Many [(h, p, (2 * n) `div` (w * p) + 1, 0, iwp) | h <- workers]))
      >>= return . result
  where w = length workers

sz :: [MasterStats] -> String
sz [] = "false"
sz (mainStats : _) =
    case "size" `lookup` mainStats of
        Nothing -> "false"
        Just s  -> "{size," ++ s ++ "}"

select_par_bench :: String -> (Vertex -> GenClos) -> Vertex -> Int -> Process Result
select_par_bench "True" = par True
select_par_bench "False" = par False
select_par_bench _ = error "Invalid IWP Flag"

select_dist_bench :: String -> (Vertex -> GenClos) -> Vertex -> Int -> [NodeId] -> Process Result
select_dist_bench "True" = dist True
select_dist_bench "False" = dist False
select_dist_bench _ = error "Invalid IWP Flag"

bench_args :: String -> (Vertex -> GenClos, Int)
bench_args "short" = (gg13, 10000)
bench_args "intermediate" = (gg124, 40000)
bench_args "long" = (gg1245, 60000)
bench_args _ = error "Invalid Version"

main :: IO ()
main = do
    args <- getArgs
    case args of
        -- Sequential Orbit
        ["seq", version, host, port] -> do
            let (gnrt, n) = bench_args version
            Right t <- createTransport host port defaultTCPParameters
            node <- newLocalNode t rtable
            runProcess node $ do
                res <- seq gnrt n
                liftIO $ print res
        -- Parallel Orbit
        ["par", iwp, version, w, host, port] -> do
            let (gnrt, n) = bench_args version
            Right t <- createTransport host port defaultTCPParameters
            node <- newLocalNode t rtable
            runProcess node $ do
                let bench = select_par_bench iwp
                res <- bench gnrt n (read w :: Int)
                liftIO $ print res
        -- Distributed Orbit
        ["dist", "master", iwp, version, w, host, port] -> do
            let (gnrt, n) = bench_args version
            b <- SLN.initializeBackend host port rtable
            print $ "Starting master @ " ++ host ++ ":" ++ port ++ " with slaves:"
            SLN.startMaster b $ \slaves -> do
                let bench = select_dist_bench iwp
                liftIO $ print $ "  " ++ show slaves
                res <- bench gnrt n (read w :: Int) slaves
                SLN.terminateAllSlaves b
                liftIO $ print res
        ["dist", "slave", host, port] -> do
            b <- SLN.initializeBackend host port rtable
            print $ "Starting slave @ " ++ host ++ ":" ++ port
            SLN.startSlave b
        -- Invalid configuration
        _ -> do
            putStrLn "Usage:"
            putStrLn "  Sequential Version"
            putStrLn "    ./orbit seq [short|intermediate|long] host port"
            putStrLn "  Paraller Version"
            putStrLn "    ./orbit par [True|False] [short|intermediate|long] nWorkers host port"
            putStrLn "  Distributed Version"
            putStrLn "  - Master Node:"
            putStrLn "    ./orbit dist master [True|False] [short|intermediate|long] nWorkers host port"
            putStrLn "  - Slave Node:"
            putStrLn "    ./orbit dist slave host port"
    where rtable :: RemoteTable
          rtable = MasterWorker.__remoteTable initRemoteTable

{-
main :: IO ()
main = do
    Right t1 <- createTransport "127.0.0.1" "5050" defaultTCPParameters
    node1 <- newLocalNode t1 rtable

    Right t2 <- createTransport "127.0.0.1" "5051" defaultTCPParameters
    node2 <- newLocalNode t2 rtable

    Right t3 <- createTransport "127.0.0.1" "5052" defaultTCPParameters
    node3 <- newLocalNode t3 rtable

    runProcess node1 $ do
        res <- par_seq gg1245 2512 32 --[localNodeId node1, localNodeId node2, localNodeId node3]
        liftIO $ print res
  where rtable :: RemoteTable
        rtable = MasterWorker.__remoteTable initRemoteTable
-}

{-
import qualified Control.Distributed.Process.Backend.SimpleLocalnet as SLN
import           System.Environment                                        (getArgs)

    args <- getArgs

    case args of
        ["master", host, port] -> do
            b <- SLN.initializeBackend host port rtable
            print $ "Starting master @ " ++ host ++ ":" ++ port ++ " with slaves:"
            SLN.startMaster b $ \slaves -> do
                liftIO $ print $ "  " ++ show slaves
                res <- dist gg13 11 2 slaves
                liftIO $ print res
        ["slave", host, port] -> do
            b <- SLN.initializeBackend host port rtable
            print $ "Starting slave @ " ++ host ++ ":" ++ port
            SLN.startSlave b
-}
    -- 1 second wait. Otherwise the main thread can terminate before
    -- our messages reach the logging process or get flushed to stdio
    --threadDelay (1 * 1000000)
