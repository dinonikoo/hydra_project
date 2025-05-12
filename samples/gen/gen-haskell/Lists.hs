-- | Basic list functions

module Hydra.Lists where

import qualified Hydra.Lib.Lists as Lists
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

apply :: ([t0 -> t1] -> [t0] -> [t1])
apply fs xs = (Lists.apply fs xs)

at :: (Int -> [t0] -> t0)
at i xs = (Lists.at i xs)

bind :: ([t0] -> (t0 -> [t1]) -> [t1])
bind xs f = (Lists.bind xs f)

concat_ :: ([[t0]] -> [t0])
concat_ xss = (Lists.concat xss)

concat2 :: ([t0] -> [t0] -> [t0])
concat2 xs ys = (Lists.concat2 xs ys)

cons :: (t0 -> [t0] -> [t0])
cons x xs = (Lists.cons x xs)

filter_ :: ((t0 -> Bool) -> [t0] -> [t0])
filter_ p xs = (Lists.filter p xs)

foldl_ :: ((t1 -> t0 -> t1) -> t1 -> [t0] -> t1)
foldl_ f z xs = (Lists.foldl f z xs)

head_ :: ([t0] -> t0)
head_ xs = (Lists.head xs)

intercalate :: ([t0] -> [[t0]] -> [t0])
intercalate sep lists = (Lists.intercalate sep lists)

intersperse :: (t0 -> [t0] -> [t0])
intersperse sep xs = (Lists.intersperse sep xs)

last_ :: ([t0] -> t0)
last_ xs = (Lists.last xs)

length_ :: ([t0] -> Int)
length_ xs = (Lists.length xs)

map_ :: ((t0 -> t1) -> [t0] -> [t1])
map_ f xs = (Lists.map f xs)

null_ :: ([t0] -> Bool)
null_ xs = (Lists.null xs)

pure_ :: (t0 -> [t0])
pure_ x = (Lists.pure x)

reverse_ :: ([t0] -> [t0])
reverse_ xs = (Lists.reverse xs)

safeHead :: ([t0] -> Maybe t0)
safeHead xs = (Lists.safeHead xs)

tail_ :: ([t0] -> [t0])
tail_ xs = (Lists.tail xs)

zip_ :: ([t0] -> [t1] -> [(t0, t1)])
zip_ xs ys = (Lists.zip xs ys)

zipWith_ :: ((t0 -> t1 -> t2) -> [t0] -> [t1] -> [t2])
zipWith_ f xs ys = (Lists.zipWith f xs ys)
