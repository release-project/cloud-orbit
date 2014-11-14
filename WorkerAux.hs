--
-- orbit-int workeraux 
--
module WorkerAux (now) where

import Types
  ( Ct(..)
  )

defaultCt = Ct {
  verts_recvd = 0
, credit_retd = 0
, min_atomic_credit = 0
, last_event = 0 --master:now()
, init_idle = -1
, tail_idle = -1
, max_idle = -1
}

-- current wall clock time (in milliseconds since start of RTS)
-- FIXME get current wall clock time
now :: Int
now = 42
