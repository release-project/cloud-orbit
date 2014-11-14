--
-- orbit-int master (controlling orbit computation)
--
module Master () where

import Control.Distributed.Process
  ( ProcessId
  )
import qualified Sequential as Sq
  ( orbit
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
 , Host
 , MaybeHosts(..)
 , ParConf
 , Stats
 , Vertex
 )

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

