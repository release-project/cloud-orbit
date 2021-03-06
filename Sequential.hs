--
-- orbit-int sequential implementation
--
module Sequential( -- Types
                   Generator
                   -- Functions
                 , orbit
                 ) where

import           Data.Dequeue  (BankersDequeue, fromList, popFront, pushBack)
import           Data.Hashable (hash)

import           Table         (Freq, Vertex, VTable, freq_to_stat, get_freq,
                                insert, is_member, new, to_list)
import           Utils         (Generator, now)

type SeqConf = ([Generator], Int)
type SeqStats = [(String, String)]

-- DATA
--   Static Machine Configuration:
--     {Gs,               --list of generators
--      TableSize}        --hash table size (ie. #slots)
--
--   Statistics:
--     List of pairs where the first component is an atom, the second
--     some data. Part of the data is the fill frequency of the table
--     (a list whose ith element indicates frequency of filling degree i).

-- compute orbit of elements in list Xs under list of generators Gs,
-- where the hash table is of size TableSize.
-- The function returns a pair consisting of the computed orbit and a singleton
-- list of statistics (mainly runtime and fill degree of the table).
orbit :: [Generator] -> [Vertex] -> Int -> ([Vertex],  [SeqStats])
orbit gs xs tableSize = (to_list finalTable, [stat])
  where -- assemble static configuration
        staticMachConf = mk_static_mach_conf gs tableSize
        -- initialise hash table and work queue
        table = new tableSize
        queue = fromList xs
        -- start wall clock timer
        startTime = now
        -- start vertex server and compute orbit
        (finalTable, vertsRecvd) = vertex_server staticMachConf table queue 0
        -- measure elapsed time (in milliseconds)
        elapsedTime = now - startTime
        -- return result
        stat = seq_stats elapsedTime (get_freq finalTable) vertsRecvd

-- main loop working off work Queue;
-- StaticMachConf -- static data
-- Table          -- hash table holding vertices
-- Queue          -- work queue
-- VertsRecvd     -- number of vertices removed from the Queue so far
vertex_server :: SeqConf -> VTable -> BankersDequeue Vertex -> Int
                 -> (VTable, Int)
vertex_server staticMachConf table queue vertsRecvd =
    case popFront queue of
        (Nothing, _)     -> (table, vertsRecvd)
        (Just x, queue1) ->
            let (newTable, newQueue) = handle_vertex staticMachConf x table queue1
            in vertex_server staticMachConf newTable newQueue (vertsRecvd + 1)

-- handle_vertex checks whether vertex X is stored in Table;
-- if not, it is in inserted and the images of the generators
-- are pushed into the work queue.
handle_vertex :: SeqConf -> Vertex -> VTable -> BankersDequeue Vertex
                 -> (VTable, BankersDequeue Vertex)
handle_vertex staticMachConf x table queue =
    case is_member x slot table of                -- check whether X is already in Table
        -- X already in table; do nothing
        True  -> (table, queue)
        -- X not in table
        False ->
            let -- insert X at Slot
                newTable = insert x slot table
                -- compute images of X under generators Gs and enqueue
                xs = [g x | g <- get_gens staticMachConf]
                newQueue = foldl pushBack queue xs
            in (newTable, newQueue) -- return updated table and queue
  where slot = hash_vertex staticMachConf x -- compute Slot

-- hash_vertex computes the hash table slot of vertex X
hash_vertex :: SeqConf -> Vertex -> Int
hash_vertex staticMachConf x = hsh `rem` tableSize
  where tableSize = get_table_size staticMachConf
        hsh = abs $ hash x

-------------------------------------------------------------------------------
-- auxiliary functions

-- functions operating on the StaticMachConf
mk_static_mach_conf :: [Generator] -> Int -> SeqConf
mk_static_mach_conf gs tableSize = (gs, tableSize)

get_gens :: SeqConf -> [Generator]
get_gens staticMachConf = fst staticMachConf

get_table_size :: SeqConf -> Int
get_table_size staticMachConf = snd staticMachConf

-- produce readable statistics
seq_stats :: Int -> Freq -> Int -> SeqStats
seq_stats elapsedTime frequency vertsRecvd =
      ("wall_time", show elapsedTime)
    : ("vertices_recvd", show vertsRecvd)
    : freq_to_stat frequency
