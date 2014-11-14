--
-- orbit-int sequential implementation
--
module Sequential(Generator
                , orbit) where

import Data.Hashable
  ( hash
  )
import Data.Dequeue
  ( BankersDequeue
  , fromList
  , popFront
  , pushBack
  )
import Table
  ( get_freq
  , freq_to_stat
  , is_member
  , insert
  , new
  , to_list
  )
import WorkerAux
  ( now
  )
import Types
  ( Generator
  , Freq
  , SeqConf
  , Stats
  , Vertex
  , VTable
  )

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
orbit :: [Generator] -> [Vertex] -> Int -> ([Vertex],  [Stats])
orbit gs xs tableSize = (orbit, [stat])
        -- assemble static configuration
  where staticMachConf = mk_static_mach_conf gs tableSize  
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
        orbit = to_list finalTable
        stat = seq_stats elapsedTime (get_freq finalTable) vertsRecvd

-- main loop working off work Queue;
-- StaticMachConf -- static data
-- Table          -- hash table holding vertices
-- Queue          -- work queue
-- VertsRecvd     -- number of vertices removed from the Queue so far
vertex_server :: SeqConf -> VTable -> BankersDequeue Vertex -> Int -> (VTable, Int)
vertex_server staticMachConf table queue vertsRecvd =
  case popFront queue of
    (Nothing, _)     -> (table, vertsRecvd)
    (Just x, queue1) -> vertex_server staticMachConf newTable newQueue (vertsRecvd + 1)
      where (newTable, newQueue) = handle_vertex staticMachConf x table queue1

-- handle_vertex checks whether vertex X is stored in Table;
-- if not, it is in inserted and the images of the generators
-- are pushed into the work queue.
handle_vertex :: SeqConf -> Vertex -> VTable -> BankersDequeue Vertex -> (VTable, BankersDequeue Vertex)
handle_vertex staticMachConf x table queue =
  case is_member x slot table of                -- check whether X is already in Table
    -- X already in table; do nothing
    True  -> (table, queue)
    -- X not in table
    False -> (newTable, newQueue)               -- return updated table and queue
      where newTable = insert x slot table      -- insert X at Slot
            xs = [g x | g <- gs]                -- compute images of X under generators Gs
            newQueue = foldl pushBack queue xs  -- enquene Xs
  where gs = get_gens staticMachConf
        slot = hash_vertex staticMachConf x     -- compute Slot


-- hash_vertex computes the hash table slot of vertex X
hash_vertex :: SeqConf -> Vertex -> Int
hash_vertex staticMachConf x =
  hsh `rem` tableSize
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
seq_stats :: Int -> Freq -> Int -> Stats
seq_stats elapsedTime frequency vertsRecvd =
  ("wall_time", show elapsedTime) : ("vertices_recvd", show vertsRecvd) : freq_to_stat frequency

