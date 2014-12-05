module Bench( -- sequential benchmarks
              seq
              -- parallel benchmarks
            , par, par_seq
              -- distributed benhcmarks
            , dist, dist_seq
            , main
            ) where

import           Control.Distributed.Process
import qualified Control.Distributed.Process.Backend.SimpleLocalnet as SLN
import           Control.Distributed.Process.Node
import           Prelude                                                    hiding (seq)
import           Network.Transport.TCP
import           System.Environment                                                (getArgs)

import           MasterWorker
import           Utils

-----------------------------------------------------------------------------
-- benchmarks, parametrised by
-- * list of Generators
-- * size of space N > 0
-- * number of processors P > 0 (per node)
-- * list of Workers (in short node name format 'name@host')
-- sequential orbit computation
seq :: (Vertex -> GenClos) -> Vertex -> Process String
seq generators n =
    orbit (generators n) [0] (Seq (2 * n)) >>= return . sz . snd

-- parallel orbit computation (par_seq/3 does not spawn image computation)
par :: (Vertex -> GenClos) -> Vertex -> Int -> Process String
par generators n p =
     orbit (generators n) [0]
       (Par (JustOne (p, ((2 * n) `div` p) + 1, 0, True)))
       >>= return . sz . snd

par_seq :: (Vertex -> GenClos) -> Vertex -> Int -> Process String
par_seq generators n p =
    orbit (generators n) [0]
      (Par (JustOne (p, ((2 * n) `div` p) + 1, 0, False)))
      >>= return . sz . snd

-- distributed orbit computation (dist_seq/4 does not spawn image computation)
dist :: (Vertex -> GenClos) -> Vertex -> Int -> [NodeId] -> Process String
dist generators n p workers =
    orbit (generators n) [0]
      (Par (Many [(h, p, (2 * n) `div` (w * p) + 1, 0, True) | h <- workers]))
      >>= return . sz . snd
  where w = length workers

dist_seq :: (Vertex -> GenClos) -> Vertex -> Int -> [NodeId] -> Process String
dist_seq generators n p workers =
    orbit (generators n) [0]
      (Par (Many [(h, p, (2 * n) `div` (w * p) + 1, 0, False) | h <- workers]))
      >>= return . sz . snd
  where w = length workers

sz :: [MasterStats] -> String
sz [] = "false"
sz (mainStats : _) =
    case "size" `lookup` mainStats of
        Nothing -> "false"
        Just s  -> "{size," ++ s ++ "}"

select_par_bench :: String -> (Vertex -> GenClos) -> Vertex -> Int -> Process String
select_par_bench "True" = par
select_par_bench "False" = par_seq
select_par_bench _ = error "Invalid IWP Flag"

select_dist_bench :: String -> (Vertex -> GenClos) -> Vertex -> Int -> [NodeId] -> Process String
select_dist_bench "True" = dist
select_dist_bench "False" = dist_seq
select_dist_bench _ = error "Invalid IWP Flag"

bench_args :: String -> (Vertex -> GenClos, Int)
bench_args "short" = (gg13, 11)
bench_args "intermediate" = (gg124, 157)
bench_args "long" = (gg1245, 157)
bench_args _ = error "Invalid Version"

main :: IO ()
main = do
    args <- getArgs
    case args of
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
                liftIO $ print res
        ["dist", "slave", host, port] -> do
            b <- SLN.initializeBackend host port rtable
            print $ "Starting slave @ " ++ host ++ ":" ++ port
            SLN.startSlave b
        -- Invalid configuration
        _ -> do
            putStrLn "Paraller Version"
            putStrLn "Usage: ./orbit par [True|False] [short|intermediat|long] nWorkers host port"
            putStrLn "Distributed Version [Master Node]"
            putStrLn "Usage: ./orbit dist master [True|False] [short|intermediat|long] nWorkers host port"
            putStrLn "Distributed Version [Slave Node]"
            putStrLn "Usage: ./orbit dist slave host port"
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
