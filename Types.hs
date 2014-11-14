--
-- orbit-int types
--
module Types ( Ct(..)
             , Generator
             , Freq
             , Host
             , MaybeHosts(..)
             , ParConf
             , SeqConf
             , Stats
             , Vertex
             , VTable) where

import Data.Array
  ( Array
  )
import Control.Distributed.Process
  ( NodeId
  , ProcessId
  )

type Freq   = [Int]
type Vertex = Int
type VTable = Array Int [Vertex]
type Stats  = [(String, String)]
type Generator = Vertex -> Vertex
type Host = (NodeId, Int, Int, Int) -- Node, Procs, TableSize, IdleTimeout
data MaybeHosts = Seq Int
                | Par [Host]
type SeqConf = ([Generator], Int)
type ParConf = ([Generator], ProcessId, [ProcessId], Int, Int, Bool)

-- counters/timers record
data Ct = Ct {
    verts_recvd :: Int        -- #vertices received by this server so far
  , credit_retd :: Int        -- #times server has returned credit to master
  , min_atomic_credit :: Int  -- minimal atomic credit received so far
  , last_event  :: Int        -- time stamp [ms] of most recent event
  , init_idle   :: Int        -- idle time [ms] between init recv first vertex
  , tail_idle   :: Int        -- idle time [ms] between send last vertex and dump
  , max_idle    :: Int        -- max idle [ms] time between vertices
}

