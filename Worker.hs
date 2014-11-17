--
-- orbit-int worker
--
module Worker( defaultCt
             , now
             , worker_stats
             , verts_recvd_from_stat
             , credit_retd_from_stat
             , min_atomic_credit_from_stat
             , init_idle_from_stat
             , tail_idle_from_stat
             , max_idle_from_stat
             ) where

import           Control.Distributed.Process (NodeId)
import           Table                       (freq_to_stat)
import           Types                       (Ct (..), Freq, Stats)

defaultCt :: Ct
defaultCt = Ct { verts_recvd = 0
               , credit_retd = 0
               , min_atomic_credit = 0
               , last_event = now
               , init_idle = -1
               , tail_idle = -1
               , max_idle = -1
               }

-- current wall clock time (in milliseconds since start of RTS)
-- FIXME get current wall clock time
now :: Int
now = 42

-- produce readable statistics
worker_stats :: NodeId -> Freq -> Ct -> Stats
worker_stats node frequency statData = ("node", show node)
                                     : ("vertices_recvd", show $ verts_recvd statData)
                                     : ("credit_retd", show $ credit_retd statData)
                                     : ("min_atomic_credit", show $ min_atomic_credit statData)
                                     : ("init_idle_time", show $ init_idle statData)
                                     : ("max_idle_time", show $ max_idle statData)
                                     : ("tail_idle_time", show $ tail_idle statData)
                                     : freq_to_stat frequency

verts_recvd_from_stat :: Stats -> Int
verts_recvd_from_stat stat =
  case "vertices_recvd" `lookup` stat of
    Just val -> read val :: Int
    Nothing  -> 0  -- instead of false

credit_retd_from_stat :: Stats -> Int
credit_retd_from_stat stat =
  case "credit_retd" `lookup` stat of
    Just val -> read val :: Int
    Nothing  -> 0  -- instead of false

min_atomic_credit_from_stat :: Stats -> Int
min_atomic_credit_from_stat stat =
  case "min_atomic_credit" `lookup` stat of
    Just val -> read val :: Int
    Nothing  -> 0  -- instead of false

init_idle_from_stat :: Stats -> Int
init_idle_from_stat stat =
  case "init_idle_time" `lookup` stat of
    Just val -> read val :: Int
    Nothing  -> 0  -- instead of false

tail_idle_from_stat :: Stats -> Int
tail_idle_from_stat stat =
  case "tail_idle_time" `lookup` stat of
    Just val -> read val :: Int
    Nothing  -> 0  -- instead of false

max_idle_from_stat :: Stats -> Int
max_idle_from_stat stat =
  case "max_idle_time" `lookup` stat of
    Just val -> read val :: Int
    Nothing  -> 0  -- instead of false

