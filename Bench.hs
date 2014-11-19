module Bench( -- sequential benchmarks
              seq
              -- parallel benchmarks
            , par, par_seq
              -- distributed benhcmarks
            , dist, dist_seq
            ) where

import Data.List           (lookup)
import Data.Maybe          (fromMaybe)
import Prelude      hiding (seq)

import MasterWorker        (HostInfo(..), MaybeHosts(..), orbit)
import Utils

-----------------------------------------------------------------------------
-- benchmarks, parametrised by
-- * list of Generators
-- * size of space N > 0
-- * number of processors P > 0 (per node)
-- * list of Workers (in short node name format 'name@host')
-- sequential orbit computation
seq generators n =
    sz $ orbit (generators n) [0] (Seq (2 * n))

-- parallel orbit computation (par_seq/3 does not spawn image computation)
par generators n p =
    sz $ orbit (generators n) [0]
      (Par (JustOne (p, ((2 * n) `div` p) + 1, 0, True)))

par_seq generators n p =
    sz $ orbit (generators n) [0]
      (Par (JustOne (p, ((2 * n) `div` p) + 1, 0, False)))

-- distributed orbit computation (dist_seq/4 does not spawn image computation)
dist generators n p workers =
    sz $ orbit (generators n) [0]
      (Par (Many [(h, p, (2 * n) `div` (w * p) + 1, 1, True) | h <- workers]))
  where w = length workers

dist_seq generators n p workers =
    sz $ orbit (generators n) [0]
      (Par (Many [(h, p, (2 * n) `div` (w * p) + 1, 1, False) | h <- workers]))
  where w = length workers

sz (_, mainStats : _) =
    case "size" `lookup` mainStats of
        Nothing -> "false"
        Just s  -> "{size," ++ s ++ "}"
