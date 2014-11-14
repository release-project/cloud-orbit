--
-- orbit-int hash table (storing vertices on a worker)
--
module Table( Freq
            , Stats
            , VTable
            , Vertex
            , new
            , to_list
            , is_member
            , insert
            , get_freq
            , sum_freqs
            , sum_freqs2
            , freq_to_slots
            , freq_to_nonempty_slots
            , freq_to_vertices
            , max_freq
            , avg_freq
            , avg_nonempty_freq
            , freq_to_stat
            , freq_from_stat
            , fill_deg) where

import Data.Array (Array, elems, listArray, (!), (//))

type Freq   = [Int]
type Vertex = Int
type VTable = Array Int [Vertex]
type Stats  = [(String, String)]

-- Note: Hash tables have a fixed number of slots but each slot can store
-- a list of vertices. The functions is_member/3 and insert/3
-- expect its slot argument to be in range.

-- new(Size) creates a table with Size slots, each containing an empty list.
new :: Int -> VTable
new size = listArray (0, size - 1) $ cycle [[]]

-- to_list(T) converts a table T into a list of its entries.
to_list :: VTable -> [Vertex]
to_list = concat . elems

-- is_member(X, I, T) is true iff X is stored in table T at slot I.
is_member :: Vertex -> Int -> VTable -> Bool
is_member x i t = elem x (t ! i)

-- insert(X, I, T) inserts X into table T at slot I.
insert :: Vertex -> Int -> VTable -> VTable
insert x i t = t // [(i, x : t ! i)]

-- get_freq computes the fill frequency of table T;
-- the output is a list of integers where the number at position I
-- indicates how many slots of T are filled with I entries;
-- the sum of the output lists equals the number of slots of T.
get_freq :: VTable -> Freq
get_freq t = elems $ foldl (flip inc) freqArr freqs
  where freqs = map length $ elems t
        maxFreq = foldl max (head freqs) (tail freqs)
        freqArr = listArray (0, maxFreq) $ cycle [0]

-- freq_to_slots computes the number of slots from a table fill frequency.
freq_to_slots :: Freq -> Int
freq_to_slots = sum

-- freq_to_nonempty_slots computes the number of non empty slots from a table
-- fill frequency.
freq_to_nonempty_slots :: Freq -> Int
freq_to_nonempty_slots = sum . tail

-- freq_to_vertices computes the number of vertices from a table fill frequency.
freq_to_vertices :: Freq -> Int
freq_to_vertices f = snd $ foldl (\(i, x) n -> (i + 1, (i * n + x))) (0, 0) f

-- max_freq returns the maximum fill frequency.
max_freq :: Freq -> Int
max_freq f = length f - 1

-- avg_freq returns the average fill frequency
avg_freq :: Freq -> Float
avg_freq f = (fi $ freq_to_vertices f) / (fi $ freq_to_slots f)

-- avg_nonempty_freq returns the average fill frequency of non empty slots.
avg_nonempty_freq :: Freq -> Float
avg_nonempty_freq f =
    if verts > 0 then (fi verts) / (fi $ freq_to_nonempty_slots f)
    else 0.0
  where verts = freq_to_vertices f

-- fill_deg determines the filling degree of the table.
fill_deg :: Freq -> Float
fill_deg f = (fi $ freq_to_nonempty_slots f) / (fi $ freq_to_slots f)

-- sum_freqs/2 sums two fill frequencies.
sum_freqs2 :: Freq -> Freq -> Freq
sum_freqs2 [] sumF = sumF
sum_freqs2 f [] = f
sum_freqs2 (n : f) (m : sumF) = n + m : sum_freqs2 f sumF

-- sum_freqs/1 sums a list of fill frequencies.
sum_freqs :: [Freq] -> Freq
sum_freqs fs = foldl (flip sum_freqs2) [] fs

-- freq_to_stat produces a readable statistics from a table fill frequency;
-- the input frequency F is itself part of the statistics
freq_to_stat :: Freq -> Stats
freq_to_stat frequency = [ ("freq", show frequency)
                         , ("size", show $ freq_to_vertices frequency)
                         , ("slots", show $ freq_to_slots frequency)
                         , ("nonempty_slots",
                            show $ freq_to_nonempty_slots frequency)
                         , ("fill_deg", show $ fill_deg frequency)
                         , ("max_freq", show $ max_freq frequency)
                         , ("avg_freq", show $ avg_freq frequency)
                         , ("nonempty_avg_freq",
                            show $ avg_nonempty_freq frequency)
                         ]

-- freq_from_stat extracts a table fill frequency from a statistics Stat
-- (assuming Stat was produced by freq_to_stat/1, otherwise returns []);
freq_from_stat :: Stats -> Freq
freq_from_stat stat =
  case "freq" `lookup` stat of
    Just val -> read val :: [Int]
    Nothing -> []

--------------------------------------------------------------------------------
-- auxiliary functions
inc :: Int -> Array Int Int -> Array Int Int
inc i t = t // [(i, t ! i + 1)]

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
