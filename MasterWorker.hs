{-# LANGUAGE TemplateHaskell,BangPatterns #-}
--
-- orbit-int master (controlling orbit computation)
--
module MasterWorker( -- Master
                     HostInfo (..)
                   , MasterStats
                   , MaybeHosts(..)
                   , orbit
                   , get_gens, get_master, get_workers, get_spawn_img_comp
                   , get_global_table_size, get_idle_timeout
                   , set_idle_timeout, clear_spawn_img_comp
                     -- Worker
                   , init
                   , distribute_vertices
                   , send_image
                   , verts_recvd_from_stat
                   , credit_retd_from_stat
                   , min_atomic_credit_from_stat
                   , init_idle_from_stat
                   , tail_idle_from_stat
                   , max_idle_from_stat
                   , WorkerStats
                   , __remoteTable
                   ) where

import           Control.Distributed.Process
import           Control.Distributed.Process.Closure
import           Data.Hashable                              (hash)
import           Data.Maybe                                 (fromJust)
import           Prelude                             hiding (init)

import           Credit
import qualified Sequential                          as Sq  (orbit)
import           Table
import           Utils                                      (GenClos (..),
                                                             Generator,
                                                             now)

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
type ParConf =
    (GenClos, ProcessId, [(ProcessId, Int, Int)], Int, Int, Bool)

type WorkerStats = [(String, String)]

-------------------------------------------------------------------------------
-- auxiliary functions

-- functions operating on the StaticMachConf
mk_static_mach_conf :: GenClos -> ProcessId -> [(ProcessId, Int, Int)] -> Int
                       -> ParConf
mk_static_mach_conf gs master workers globalTableSize =
    (gs, master, workers, globalTableSize, 0, True)

get_gens :: ParConf -> GenClos
get_gens (gs, _, _, _, _, _) = gs

get_master :: ParConf -> ProcessId
get_master (_, master, _, _, _, _) = master

get_workers :: ParConf -> [(ProcessId, Int, Int)]
get_workers (_, _, workers, _, _, _) = workers

get_global_table_size :: ParConf -> Int
get_global_table_size (_, _, _, globalTableSize, _, _) = globalTableSize

get_idle_timeout :: ParConf -> Int
get_idle_timeout (_, _, _, _, timeout, _) = timeout

get_spawn_img_comp :: ParConf -> Bool
get_spawn_img_comp (_, _, _, _, _, spawmImgComp) = spawmImgComp


set_idle_timeout :: ParConf -> Int -> ParConf
set_idle_timeout (gs, mst, wks, gts, _, spic) x =
    (gs, mst, wks, gts, x, spic)

clear_spawn_img_comp :: ParConf -> ParConf
clear_spawn_img_comp (gs, mst, wks, gts, tmt, _) =
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

--
-- orbit-int worker (computing vertices and holding part of hash table)
--

defaultCt :: Ct
defaultCt = Ct { verts_recvd = 0
               , credit_retd = 0
               , min_atomic_credit = 0
               , last_event = now
               , init_idle = -1
               , tail_idle = -1
               , max_idle = -1
               }

-- initialise worker
init :: (Int, Int, Bool) -> Process ()
init (localTableSize, idleTimeout, spawnImgComp) =
    receiveWait [
        match $ \("init", staticMachConf0) -> do
            let staticMachConf1 = set_idle_timeout staticMachConf0 idleTimeout
                staticMachConf =
                    if spawnImgComp then staticMachConf1
                                    else clear_spawn_img_comp staticMachConf1
            vertex_server staticMachConf zero (new localTableSize) defaultCt
      ]

-- main worker loop: server handling vertex messages;
-- StaticMachConf: info about machine configuration
-- Credit: credit currently held by the server,
-- Table: hash table holding vertices
-- StatData: various counters and timers for gathering statistics
vertex_server :: ParConf -> Credit -> VTable -> Ct -> Process ()
vertex_server staticMachConf crdt table !statData = do
    let idleTimeout = get_idle_timeout staticMachConf
    r <- receiveTimeout idleTimeout [
        match $ \("vertex", x, slot, k) -> do
            -- say $ "Got a vertex!"
            return $ Just (x, slot, k)
      , match $ \("dump") -> do
            return $ Nothing
      ]
    case r of
        Nothing -> do let creditRetd = credit_retd statData
                      newCreditRetd <- return_credit staticMachConf crdt creditRetd
                      let newStatData = statData {credit_retd = newCreditRetd}
                      vertex_server staticMachConf zero table newStatData
        Just msg -> case msg of
          Just (x, slot, k) -> do
            let creditPlusK = credit_atomic k crdt
                nowTime = now
                vertsRecvd = verts_recvd statData
                minAtomicCredit = min_atomic_credit statData
                lastEvent = last_event statData
                initIdle = init_idle statData
                maxIdle = max_idle statData
            (newCredit, newTable) <-
                handle_vertex staticMachConf x slot creditPlusK table
            let newStatData0 = statData {verts_recvd = vertsRecvd + 1,
                                         min_atomic_credit = max minAtomicCredit k}
                newStatData1 =
                    if initIdle < 0
                        then newStatData0 {init_idle = nowTime - lastEvent}
                        else newStatData0 {max_idle = max maxIdle (nowTime - lastEvent)}
                newStatData = newStatData1 {last_event = now}
            vertex_server staticMachConf newCredit newTable newStatData
          Nothing -> do
            let nowTime = now
                lastEvent = last_event statData
                newStatData = statData {tail_idle = nowTime - lastEvent,
                                        last_event = now}
            dump_table staticMachConf table newStatData

-- handle_vertex checks whether vertex X is stored in Slot of Table;
-- if not, it is in inserted there and the images of the generators
-- are distributed among the workers.
-- Precondition: Credit is non-zero.
handle_vertex :: ParConf -> Vertex -> Int -> Credit -> VTable
                 -> Process (Credit, VTable)
handle_vertex staticMachConf x slot crdt table
    | is_member x slot table = return (crdt, table) -- x already in table;
                                                    -- do nothing
    | otherwise = do -- x not in table
        let newTable = insert x slot table -- insert x at slot
        -- distribute images of x under generators to their respective workers
        newCredit <- distribute_images staticMachConf x crdt
        -- return remaining credit and updated table
        return (newCredit, newTable)

-- return_credit sends non-zero Credit back to the master;
-- returns number of times credit has been returned so far
return_credit :: ParConf -> Credit -> Int -> Process Int
return_credit staticMachConf crdt creditRetd
    | is_zero crdt = return creditRetd
    | otherwise      = do
         let masterPid = get_master staticMachConf
         send masterPid ("done", crdt)
         return (creditRetd + 1)

-- dump_table sends a list containing the local partial orbit to the master,
-- together with some statistics on the distribution of vertices in the table.
dump_table :: ParConf -> VTable -> Ct -> Process ()
dump_table staticMachConf table statData = do
    nodeId <- getSelfNode
    let masterPid = get_master staticMachConf
        stat = worker_stats nodeId (get_freq table) statData
    send masterPid ("result", to_list table, stat)

-- distribute_images distributes the images of vertex X under the generators
-- to the workers determined by the hash; some ore all of of the Credit is
-- used to send the messages, the remaining credit is returned;
-- computation and sending of vertices is actually done asynchronously.
-- Precondition: Credit is non-zero.
distribute_images :: ParConf -> Vertex -> Credit -> Process Credit
distribute_images staticMachConf x crdt =
    do_distribute_images staticMachConf x crdt (get_gens staticMachConf)

do_distribute_images :: ParConf -> Vertex -> Credit -> GenClos
                        -> Process Credit
do_distribute_images _ _ crdt (GenClos (_, _, [])) =
    return crdt
do_distribute_images staticMachConf x crdt (GenClos (_, _, [g])) = do
    let (k, remainingCredit) = debit_atomic crdt
    if get_spawn_img_comp staticMachConf
        then spawnLocal (send_image staticMachConf x g k) >> return ()
        else send_image staticMachConf x g k
    return remainingCredit
do_distribute_images staticMachConf x crdt (GenClos (name, n, g : gs)) = do
    let (k, nonZeroRemainingCredit) = debit_atomic_nz crdt
    if get_spawn_img_comp staticMachConf
        then spawnLocal (send_image staticMachConf x g k) >> return ()
        else send_image staticMachConf x g k
    do_distribute_images staticMachConf x nonZeroRemainingCredit
      (GenClos (name, n, gs))

-- distribute_vertices distributes the list of vertices Xs to the workers
-- determined by the hash; some ore all of of the Credit is used to send
-- the messages, the remaining credit is returned.
-- Precondition: If Xs is non-empty then Credit must be non-zero.
distribute_vertices :: ParConf -> Credit -> Credit -> Process Credit
distribute_vertices _ crdt [] = return crdt
distribute_vertices staticMachConf crdt [x] = do
    let (k, remainingCredit) = debit_atomic crdt
    -- say $ "Remaining credit = " ++ show remainingCredit ++ " k = " ++ show k
    send_vertex staticMachConf x k
    return remainingCredit
distribute_vertices staticMachConf crdt (x : xs) = do
    let (k, nonZeroRemainingCredit) = debit_atomic_nz crdt
    send_vertex staticMachConf x k
    distribute_vertices staticMachConf nonZeroRemainingCredit xs

-- send_image sends image of X under G to the worker determined by
-- the hash of G(X); the message is tagged with atomic credit K.
send_image :: ParConf -> Vertex -> Generator -> ACredit -> Process ()
send_image staticMachConf x g k = send_vertex staticMachConf (g x) k

-- send_vertex hashes vertex X and sends it to the worker determined by
-- the hash; the message is tagged with atomic credit K.
send_vertex :: ParConf -> Vertex -> ACredit -> Process ()
send_vertex staticMachConf x k = do
    --say $ "Send vertex " ++ show x ++ " to " ++ show pid ++ " (slot, k) = " ++ show (slot, k)
    send pid ("vertex", x, slot, k)
  where (pid, slot) = hash_vertex staticMachConf x

-- hash_vertex computes the two-dimensional hash table slot of vertex X where
-- the first dim is a worker pid and the second a slot in that worker's table.
hash_vertex :: ParConf -> Vertex -> (ProcessId, Int)
hash_vertex staticMachConf x = global_to_local_slot workers globalSlot
  where -- get static info
        globalTableSize = get_global_table_size staticMachConf
        workers = get_workers staticMachConf
        -- compute raw hash and slot in global table
        globalSlot = (hash x) `rem` globalTableSize

-- global_to_local_slot traverses the list Workers sequentially to translate
-- slot GlobSlot in the global hash table into a two-dimensional local slot
-- {pid, slot}, where 'pid' is the PID of a worker and 'slot' a the slot
-- in that worker's local hash table.
-- Precondition: GlobSlot < sum of TableSize in Workers.
-- Note: This procedure is horribly inefficient (linear in size of Workers);
-- it should be log (size of Workers) at most.
global_to_local_slot :: [(ProcessId, Int, Int)] -> Int -> (ProcessId, Int)
global_to_local_slot [] _ = error "global_to_local_slot cannot work on [] workers."
global_to_local_slot ((pid, _, tabSize) : workers) globSlot
    | globSlot < tabSize = (pid, globSlot)
    | otherwise = global_to_local_slot workers (globSlot - tabSize)

-------------------------------------------------------------------------------
-- auxiliary functions

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

remotable ['init]

--
-- orbit-int master (controlling orbit computation)
--

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

orbit :: GenClos -> [Vertex] -> MaybeHosts -> Process ([Vertex], [MasterStats])
orbit (GenClos (_, _, gs)) xs (Seq tablesize) =
    return $ Sq.orbit gs xs tablesize
orbit gs xs (Par hostInfo) = par_orbit gs xs hostInfo

par_orbit :: GenClos -> [Vertex] -> HostInfo
             -> Process ([Vertex], [MasterStats])
par_orbit gs xs hosts = do
    -- say "---- In par_orbit"
    -- spawn workers on Hosts
    (workers, globTabSize) <- start_workers hosts
    self <- getSelfPid
    let -- assemble StaticMachConf and distribute to Workers
        staticMachConf = mk_static_mach_conf gs self workers globTabSize
    mapM_ (\(pid, _, _) -> send pid ("init", staticMachConf)) workers
    let -- start wall clock timer
        startTime = now
    -- distribute initial vertices to workers
    -- say $ "---- After send pid init, xs = " ++ show xs
    crdt <- distribute_vertices staticMachConf one xs
    -- say $ "---- After distribute_vertices, credit = " ++ show crdt
    -- collect credit handed back by idle workers
    collect_credit crdt
    -- say "---- After collect credit"
    -- collect credit handed back by idle workers
    let -- measure elapsed time (in milliseconds)
        elapsedTime = now - startTime
    -- tell all workers to dump their tables
    mapM_ (\(pid, _, _) -> send pid "dump") workers
    -- collect results from all workers and return them
    collect_orbit elapsedTime (length workers)

-- start_workers starts worker processes depending on the input Hosts:
-- * if Hosts is a quadruple {P, _, _, _} then P processes are forked on the
-- executing Erlang node;
-- * if Hosts is a non-empty list {H1, P1, _, _, _}, {H2, P2, _, _, _}, ...
-- then P1 processes are forked on Erlang node H1, P2 processes on node H2,
-- and so on.
-- The function returns a pair {Workers, GlobalTableSize}, where
-- * GlobalTableSize is the total number of slots of the global hash table, and
-- * Workers is a list of Worker, sorted wrt. TableOffset in ascending order.
start_workers :: HostInfo -> Process ([(ProcessId, Int, Int)], Int)
start_workers (JustOne host) = do
    -- say "---- In start_workers"
    (workers, globalTableSize) <- do_start_shm host ([], 0)
    -- say "---- After do_start_shm"
    return (workers, globalTableSize)
start_workers (Many hosts) = do
    -- say "---- In many start_workers"
    (workers, globalTableSize) <- do_start_dist hosts ([], 0)
    -- say $ "---- After do_start_dist, Workers = " ++ show workers
    return (reverse workers, globalTableSize)

do_start_shm :: (Int, Int, Int, Bool) -> ([(SpawnRef, Int, Int)], Int)
                -> Process ([(ProcessId, Int, Int)], Int)
do_start_shm host initAcc = do
    (workers, globalTableSize) <- do_start_shm_h host initAcc
    nWorkers <- wait_workers_shm workers []
    return (nWorkers, globalTableSize)

do_start_shm_h :: (Int, Int, Int, Bool) -> ([(SpawnRef, Int, Int)], Int)
                -> Process ([(SpawnRef, Int, Int)], Int)
do_start_shm_h (0, _, _, _) acc = return acc
do_start_shm_h (m, tabSize, tmOut, spawnImgComp) (workers, gTabSize) = do
    node <- getSelfNode
    sRef <- spawnAsync node ($(mkClosure 'init) (tabSize, tmOut, spawnImgComp))
    do_start_shm_h (m - 1, tabSize, tmOut, spawnImgComp)
      ((sRef, gTabSize, tabSize) : workers, gTabSize + tabSize)

wait_workers_shm :: [(SpawnRef, Int, Int)] -> [(ProcessId, Int, Int)]
                    -> Process [(ProcessId, Int, Int)]
wait_workers_shm [] acc = return acc
wait_workers_shm ((sRef, gTabSize, tabSize) : workers) acc = do
    mPid <- receiveWait
        [ matchIf (\(DidSpawn ref _) -> ref == sRef)
                  (\(DidSpawn _ pid) -> return pid)
        ]
    wait_workers_shm workers ((mPid, gTabSize, tabSize) : acc)

do_start_dist :: [(NodeId, Int, Int, Int, Bool)] -> ([(ProcessId, Int, Int)], Int)
                 -> Process ([(ProcessId, Int, Int)], Int)
do_start_dist [] acc = return acc
do_start_dist ((_, 0, _, _, _) : hosts) acc = do_start_dist hosts acc
do_start_dist ((node,m,tabSize,tmOut,spawnImgComp) : hosts) (workers,gTabSize) = do
    pid <- spawn node ($(mkClosure 'init) (tabSize, tmOut, spawnImgComp))
    do_start_dist ((node, m - 1, tabSize, tmOut, spawnImgComp) : hosts)
      ((pid, gTabSize, tabSize) : workers, gTabSize + tabSize)

-- collect_credit collects leftover credit from idle workers until
-- the credit adds up to 1.
collect_credit :: Credit -> Process ()
collect_credit crdt
    | is_one crdt = return ()
    | otherwise   = receiveWait [
                        match $ \("done", workersCredit) ->
                          collect_credit $ credit workersCredit crdt
                      ]

-- collect_orbit collects partial orbits and stats from N workers.
collect_orbit :: Int -> Int -> Process ([Vertex], [MasterStats])
collect_orbit elapsedTime n = do
    (orb, stats) <- do_collect_orbit n [] []
    return (concat orb, master_stats elapsedTime stats : stats)

do_collect_orbit :: Int -> [[Vertex]] -> [WorkerStats]
                    -> Process ([[Vertex]], [WorkerStats])
do_collect_orbit 0 partOrbits workerStats = return (partOrbits, workerStats)
do_collect_orbit n partOrbits workerStats = do
    receiveWait [
        match $ \("result", partOrbit, workerStat) ->
          do_collect_orbit (n - 1) (partOrbit : partOrbits) (workerStat : workerStats)
      ]
