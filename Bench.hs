module Bench( -- sequential benchmarks
              seq
              -- parallel benchmarks
            , par, par_seq
              -- distributed benhcmarks
            , dist, dist_seq
            , seqTest
            ) where

import Control.Concurrent                      (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Prelude                          hiding (seq)
import Network.Transport.TCP

import MasterWorker                            (HostInfo(..), MaybeHosts(..),
                                                MasterStats, orbit)
import Utils

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
      (Par (Many [(h, p, (2 * n) `div` (w * p) + 1, 1, True) | h <- workers]))
      >>= return . sz . snd
  where w = length workers

dist_seq :: (Vertex -> GenClos) -> Vertex -> Int -> [NodeId] -> Process String
dist_seq generators n p workers =
    orbit (generators n) [0]
      (Par (Many [(h, p, (2 * n) `div` (w * p) + 1, 1, False) | h <- workers]))
      >>= return . sz . snd
  where w = length workers

sz :: [MasterStats] -> String
sz [] = "false"
sz (mainStats : _) =
    case "size" `lookup` mainStats of
        Nothing -> "false"
        Just s  -> "{size," ++ s ++ "}"

seqTest :: IO ()
seqTest = do
    Right t <- createTransport "127.0.0.1" "10504" defaultTCPParameters
    node <- newLocalNode t initRemoteTable
    runProcess node $ do
        res <- par gg13 11 2
        liftIO $ print res
    threadDelay (1 * 1000000)
