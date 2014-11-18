--
-- orbit-int master (controlling orbit computation)
-- orbit-int worker (computing vertices and holding part of hash table)
--
module MasterWorker where
{-
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
-}

import           Control.Distributed.Process (Process, ProcessId, NodeId,
                                              match, receiveWait)
import           Data.Maybe                  (fromJust)

import           Credit                      (credit, is_one)
import qualified Sequential                  as Sq (Generator, orbit)
import           Table                       (Freq, Vertex, freq_from_stat,
                                              freq_to_stat, sum_freqs)
import           Utils                       (now)

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
type MasterStats = [(String, String)]
data MaybeHosts = Seq Int
                | Par HostInfo
data HostInfo = JustOne (Int,  -- Number of processes
                         Int,  -- Table size
                         Int,  -- Idle timeout
                         Bool) -- Spawn image comp
              | Many    [(NodeId, -- Node id
                          Int,    -- Number of processes
                          Int,    -- Table size
                          Int,    -- Idle timeout
                          Bool)]  -- Spawn image comp
type ParConf = ([Sq.Generator], ProcessId, [ProcessId], Int, Int, Bool)

type WorkerStats = [(String, String)]

-- DATA
--   Static Machine Configuration:
--     {Gs,               %list of generators
--      Master,           %pid of master process
--      Workers,          %list of Worker
--      GlobalTableSize,  %size of global hash table
--      IdleTimeout,      %milliseconds this worker idles before sending 'done'
--      SpawnImgComp}     %true iff this worker spawns image computations
--
--   Worker:
--     {Pid,              %pid of worker process
--      TableOffset,      %offset (= index 0) of local table into global table
--      TableSize}        %size of local hash table
--
--   Host:
--     {Node,             %atom naming Erlang node
--      Procs,            %number of processors
--      TableSize,        %size of hash table per processor
--      IdleTimeout}      %milliseconds a processor idles before sending 'done'
--
--   Statistics:
--     List of pairs where the first component is an atom, the second
--     some data. Part of the data is the fill frequency of the table
--     (a list whose ith element indicates frequency of filling degree i).


-- MESSAGES
--   Master -> Worker:        {init, StaticMachConf}
--
--   Master/Worker -> Worker: {vertex, X, Slot, K}
--                               %X is vertex
--                               %Slot is slot of X on target worker
--                               %K is atomic credit shipped with vertex
--
--   Worker -> Master:        {done, Cs}
--                               %Cs is non-zero credit (rep as list of ints)
--
--   Master -> Worker:        {dump}
--
--   Worker -> Master:        {result, Xs, Stats}
--                               %Xs is list of found orbit vertices
--                               %Stats is statistics about worker's table


-- compute orbit of elements in list Xs under list of generators Gs;
-- the argument Hosts is either an integer N, a triple {P, N, T}, or
-- a non-empty list [{H, P, N, T} | ...] of quadruples:
-- * N:                       run the sequential algorithm with table size N
-- * {P, N, T, S}:            run the parallel algorithm on P processors
--                            each with table size N, idle timeout T and
--                            spawn image computation flag S;
-- * [{H, P, N, T, S} | ...]: run the distributed algorithm on the list of
--                            hosts, where each quintuple {H, P, N, T, S}
--                            specifies
--                            * host name H (ie. name of Erlang node),
--                            * number of processors P on H,
--                            * table size N (per processor),
--                            * idle timeout T, and
--                            * spawn image computation flag S.
-- The function returns a pair consisting of the computed orbit and
-- a list of statistics, the first element of which reports overall statistics,
-- and all remaining elements report statistics of some worker.
orbit :: [Vertex -> Vertex] -> [Vertex] -> MaybeHosts
         -> ([Vertex],  [MasterStats])
orbit gs xs (Seq tablesize) = Sq.orbit gs xs tablesize
orbit gs xs (Par hostInfo)     = par_orbit gs xs hostInfo

-- FIXME Write the proper par_orbit
par_orbit :: [Vertex -> Vertex] -> [Vertex] -> HostInfo
             -> ([Vertex],  [MasterStats])
par_orbit gs xs hosts = ([42], [[("xxx", "xxx")]])

-- collect_credit collects leftover credit from idle workers until
-- the credit adds up to 1.
collect_credit :: [Int] -> Process ()
collect_credit crdt =
    case is_one crdt of
        True  -> return ()
        False -> receiveWait [
                     match $ \("done", workersCredit) ->
                       collect_credit $ credit workersCredit crdt
                   ]

-- collect_orbit collects partial orbits and stats from N workers.
collect_orbit :: Int -> Int -> Process ([Vertex], [MasterStats])
collect_orbit elapsedTime n = do
    (orbit, stats) <- do_collect_orbit n [] []
    return (concat orbit, master_stats elapsedTime stats : stats)

do_collect_orbit :: Int -> [[Vertex]] -> [WorkerStats]
                    -> Process ([[Vertex]], [WorkerStats])
do_collect_orbit 0 partOrbits workerStats = return (partOrbits, workerStats)
do_collect_orbit n partOrbits workerStats = do
    receiveWait [
        match $ \("result", partOrbit, workerStat) ->
          do_collect_orbit (n - 1) (partOrbit : partOrbits) (workerStat : workerStats)
      ]

-------------------------------------------------------------------------------
-- auxiliary functions

-- functions operating on the StaticMachConf
mk_static_mach_conf :: [Sq.Generator] -> ProcessId -> [ProcessId] -> Int
                       -> ParConf
mk_static_mach_conf gs master workers globalTableSize =
    (gs, master, workers, globalTableSize, 0, True)

get_gens :: ParConf -> [Sq.Generator]
get_gens (gs, _, _, _, _, _) = gs

get_master :: ParConf -> ProcessId
get_master (_, master, _, _, _, _) = master

get_workers :: ParConf -> [ProcessId]
get_workers (_, _, workers, _, _, _) = workers

get_global_table_size :: ParConf -> Int
get_global_table_size (_, _, _, globalTableSize, _, _) = globalTableSize

get_idle_timeout :: ParConf -> Int
get_idle_timeout (_, _, _, _, timeout, _) = timeout

get_spawn_img_comp :: ParConf -> Bool
get_spawn_img_comp (_, _, _, _, _, spawmImgComp) = spawmImgComp


set_idle_timeout :: ParConf -> Int -> ParConf
set_idle_timeout (gs, mst, wks, gts, timeout, spic) x =
    (gs, mst, wks, gts, x, spic)

clear_spawn_img_comp :: ParConf -> ParConf
clear_spawn_img_comp (gs, mst, wks, gts, tmt, spawmImgComp) =
    (gs, mst, wks, gts, tmt, False)

-- produce readable statistics
master_stats :: Int -> [WorkerStats] -> MasterStats
master_stats elapsedTime workerStats =
      ("wall_time", show elapsedTime)
    : ("vertices_recvd", show vertsRecvd)
    : ("credit_retd", show creditRetd)
    : ("min_atomic_credit", show minAtomicCredit)
    : ("max_init_idle_time", show maxInitIdle)
    : ("max_idle_time", show maxIdle)
    : ("max_tail_idle_time", show maxTailIdle)
    : freq_to_stat freq
  where freq = sum_freqs $ map freq_from_stat workerStats
        vertsRecvd = sum $ map verts_recvd_from_stat workerStats
        creditRetd = sum $ map credit_retd_from_stat workerStats
        atomicCredits = map min_atomic_credit_from_stat workerStats
        minAtomicCredit = foldl max (head atomicCredits) (tail atomicCredits) -- TODO WTF max ?
        initIdles = map init_idle_from_stat workerStats
        maxInitIdle = foldl max (head initIdles) (tail initIdles)
        idles = map max_idle_from_stat workerStats
        maxIdle = foldl max (head idles) (tail idles)
        tailIdles = map tail_idle_from_stat workerStats
        maxTailIdle = foldl max (head tailIdles) (tail tailIdles)

-- Worker stuff

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
