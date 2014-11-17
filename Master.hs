--
-- orbit-int master (controlling orbit computation)
--
module Master () where

import Control.Distributed.Process
  ( Process
  , ProcessId
  , receiveWait
  , match
  )
import qualified Sequential as Sq
  ( orbit
  )
import Credit
  ( is_one
  , credit
  )
import Table
  ( sum_freqs
  , freq_from_stat
  , freq_to_stat
  )
import WorkerAux
  ( verts_recvd_from_stat
  , credit_retd_from_stat
  , min_atomic_credit_from_stat
  , init_idle_from_stat
  , max_idle_from_stat
  , tail_idle_from_stat
  )
import Types
 ( Generator
 , HostInfo(..)
 , MaybeHosts(..)
 , ParConf
 , Stats
 , Vertex
 )

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
orbit :: [Vertex -> Vertex] -> [Vertex] -> MaybeHosts -> ([Vertex],  [Stats])
orbit gs xs (Seq tablesize) = Sq.orbit gs xs tablesize
orbit gs xs (Par hostInfo)     = par_orbit gs xs hostInfo


-- FIXME Write the proper par_orbit
par_orbit :: [Vertex -> Vertex] -> [Vertex] -> HostInfo -> ([Vertex],  [Stats])
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
collect_orbit :: Int -> Int -> Process ([Vertex], [Stats])
collect_orbit elapsedTime n = do
  (orbit, stats) <- do_collect_orbit n [] []
  return (concat orbit, master_stats elapsedTime stats : stats)

do_collect_orbit :: Int -> [[Vertex]] -> [Stats] -> Process ([[Vertex]], [Stats])
do_collect_orbit 0 partOrbits workerStats = return (partOrbits, workerStats)
do_collect_orbit n partOrbits workerStats = do
  receiveWait [
      match $ \("result", partOrbit, workerStat) ->
        do_collect_orbit (n - 1) (partOrbit : partOrbits) (workerStat : workerStats)
    ]

-------------------------------------------------------------------------------
-- auxiliary functions

-- functions operating on the StaticMachConf
mk_static_mach_conf :: [Generator] -> ProcessId -> [ProcessId] -> Int -> ParConf
mk_static_mach_conf gs master workers globalTableSize = (gs, master, workers, globalTableSize, 0, True)

get_gens :: ParConf -> [Generator]
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
set_idle_timeout (gs, mst, wks, gts, timeout, spic) x = (gs, mst, wks, gts, x, spic)

clear_spawn_img_comp :: ParConf -> ParConf
clear_spawn_img_comp (gs, mst, wks, gts, tmt, spawmImgComp) = (gs, mst, wks, gts, tmt, False)

-- produce readable statistics
master_stats :: Int -> [Stats] -> Stats
master_stats elapsedTime workerStats = ("wall_time", show elapsedTime)
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

