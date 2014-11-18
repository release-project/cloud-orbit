--
-- orbit-int worker (computing vertices and holding part of hash table)
--
module Worker( --init
               --, distribute_vertices
               --, send_image
               verts_recvd_from_stat
             , credit_retd_from_stat
             , min_atomic_credit_from_stat
             , init_idle_from_stat
             , tail_idle_from_stat
             , max_idle_from_stat
             , WorkerStats
             ) where

import           Control.Distributed.Process (NodeId)
import           Data.Maybe                  (fromJust)

import           Table                       (Freq, freq_to_stat)
import           Utils                       (now)

type WorkerStats = [(String, String)]

-- counters/timers record
data Ct = Ct { verts_recvd :: Int    -- #vertices received by this server so far
             , credit_retd :: Int    -- #times server has returned credit to
                                     -- master
             , min_atomic_credit :: Int  -- minimal atomic credit received so far
             , last_event  :: Int    -- time stamp [ms] of most recent event
             , init_idle   :: Int    -- idle time [ms] between init recv first
                                     -- vertex
             , tail_idle   :: Int    -- idle time [ms] between send last vertex
                                     -- and dump
             , max_idle    :: Int    -- max idle [ms] time between vertices
             }

defaultCt :: Ct
defaultCt = Ct { verts_recvd = 0
               , credit_retd = 0
               , min_atomic_credit = 0
               , last_event = now
               , init_idle = -1
               , tail_idle = -1
               , max_idle = -1
               }

-- produce readable statistics
worker_stats :: NodeId -> Freq -> Ct -> WorkerStats
worker_stats node frequency statData =
      ("node", show node)
    : ("vertices_recvd", show $ verts_recvd statData)
    : ("credit_retd", show $ credit_retd statData)
    : ("min_atomic_credit", show $ min_atomic_credit statData)
    : ("init_idle_time", show $ init_idle statData)
    : ("max_idle_time", show $ max_idle statData)
    : ("tail_idle_time", show $ tail_idle statData)
    : freq_to_stat frequency

verts_recvd_from_stat :: WorkerStats -> Int
verts_recvd_from_stat stat =
    read (fromJust ("vertices_recvd" `lookup` stat)) :: Int

credit_retd_from_stat :: WorkerStats -> Int
credit_retd_from_stat stat =
    read (fromJust ("credit_retd" `lookup` stat)) :: Int

min_atomic_credit_from_stat :: WorkerStats -> Int
min_atomic_credit_from_stat stat =
    read (fromJust ("min_atomic_credit" `lookup` stat)) :: Int

init_idle_from_stat :: WorkerStats -> Int
init_idle_from_stat stat =
    read (fromJust ("init_idle_time" `lookup` stat)) :: Int

tail_idle_from_stat :: WorkerStats -> Int
tail_idle_from_stat stat =
    read (fromJust ("tail_idle_time" `lookup` stat)) :: Int

max_idle_from_stat :: WorkerStats -> Int
max_idle_from_stat stat =
    read (fromJust ("max_idle_time" `lookup` stat)) :: Int
