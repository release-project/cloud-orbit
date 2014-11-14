--
-- orbit-int types
--
module Types ( Generator
             , Freq
             , Host
             , MaybeHosts(..)
             , Stats
             , Vertex
             , VTable) where

import Data.Array
  ( Array
  )
import Control.Distributed.Process
  ( NodeId
  )

type Freq   = [Int]
type Vertex = Int
type VTable = Array Int [Vertex]
type Stats  = [(String, String)]
type Generator = Vertex -> Vertex
type Host = (NodeId, Int, Int, Int) -- Node, Procs, TableSize, IdleTimeout
data MaybeHosts = Seq Int
                | Par [Host]
