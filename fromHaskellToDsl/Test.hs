-- | Test functions

module Hydra.Test where

import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

nullStr :: (Int -> String -> Bool)
nullStr a s = (Logic.and (Strings.isEmpty s) (Equality.equalInt32 a 0))

equality4 :: (String -> Int -> Bool)
equality4 a b = (Logic.and (Equality.equalString a "7889") (Equality.equalInt32 b 1))

equality6 :: (String -> Bool)
equality6 a = (Equality.equalString a (Strings.cat2 a a))

equality1111 :: (String -> String -> Bool)
equality1111 s d = (Equality.equalString s d)

inRange :: (Int -> Int -> Int -> Bool)
inRange x a b = (Logic.and (Equality.gteInt32 x a) (Equality.lteInt32 x b))

aaaaa1 :: (Int -> [Int] -> Int)
aaaaa1 i xs = (Math.add (Lists.at i xs) 1)

aaaaa :: [Int]
aaaaa = [
  1]

aaaaa2 :: ([t0] -> t0)
aaaaa2 i = (Lists.at (Math.add 1 2) i)

aaaaa23 :: (Int -> Int)
aaaaa23 i = (Math.add (Lists.at i [
  1,
  2,
  3]) 1)

aaaaa0 :: (t0 -> Int)
aaaaa0 i = (Math.add 1 1)

concat21 :: ([t0] -> [t0] -> [t0])
concat21 xs ys = (Lists.concat2 xs ys)

cons :: ([Int] -> [Int])
cons xs = (Lists.cons 9 xs)

head_ :: ([t0] -> t0)
head_ xs = (Lists.head xs)

length_ :: ([t0] -> Int)
length_ xs = (Lists.length xs)
