-- | Test functions

module Hydra.Test where

import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

data Person = 
  Person {
    personName :: [Int],
    personAge :: Int,
    personSurname :: String,
    personInt16 :: I.Int16,
    personInt64 :: I.Int64,
    personInt641 :: [String]}

addMe1 :: (String -> Bool)
addMe1 x = (Equality.equalString x (Strings.toUpper "123"))

addMe2 :: (String -> Bool)
addMe2 x = (Equality.equalString x (Strings.toUpper "123"))

describeNumber3 :: (Int -> Int)
describeNumber3 n = (Logic.ifElse (Equality.ltInt32 n 0) 1 (Logic.ifElse (Equality.equalInt32 n 0) 2 3))

addMe3 :: (String -> Bool)
addMe3 x = (Equality.equalString x "123")

addMe4 :: (String -> String -> Bool)
addMe4 x y = (Equality.equalString x y)

inRange6 :: (Int -> Int -> Int -> Bool)
inRange6 x a b = (Logic.and (Equality.gteInt32 x a) (Equality.lteInt32 x b))

myTest7 :: (Int -> Int -> Int -> Int)
myTest7 x y z = (Math.add (Math.add x y) z)

myTest8 :: (Int -> Int -> t0 -> Bool)
myTest8 x y z = (Logic.not (Equality.equalInt32 x y))

myTest9 :: (Bool -> Bool)
myTest9 x = (Equality.equalBoolean x True)

isEmpty10 :: (String -> Bool)
isEmpty10 str = (Strings.isEmpty str)

myConstant11 :: Int
myConstant11 = 42

isEmpty12 :: (String -> String)
isEmpty12 str = (Strings.toUpper str)

stringCatList13 :: ([String] -> String)
stringCatList13 strList = (Strings.cat strList)

toList14 :: (String -> [Int])
toList14 codes = (Strings.toList codes)

reverse15 :: ([t0] -> [t0])
reverse15 xs = (Lists.reverse xs)

myList16 :: [Int]
myList16 = [
  1,
  2,
  3]

aaa17 :: Int
aaa17 = (Math.add 5 5)

nullStr18 :: (Int -> String -> Bool)
nullStr18 a s = (Logic.and (Strings.isEmpty s) (Equality.equalInt32 a 0))

equality19 :: (String -> String -> Bool)
equality19 s d = (Equality.equalString s d)

equality421 :: (String -> Int -> Bool)
equality421 a b = (Logic.and (Equality.equalString a "7889") (Equality.equalInt32 b 1))

equality622 :: (String -> Bool)
equality622 a = (Equality.equalString a (Strings.cat2 a a))

equality623 :: (String -> Bool)
equality623 a = (Equality.equalString "456" (Strings.toUpper a))

equality111125 :: (String -> String -> Bool)
equality111125 s d = (Equality.equalString s d)

inRange26 :: (Int -> Int -> Int -> Bool)
inRange26 x a b = (Logic.and (Equality.gteInt32 x a) (Equality.lteInt32 x b))

aaaaa126 :: (Int -> [Int] -> Int)
aaaaa126 i xs = (Math.add (Lists.at i xs) 1)

aaaaa27 :: [String]
aaaaa27 = [
  "123"]

aaaaa28 :: [Int]
aaaaa28 = [
  1]

aaaaa229 :: (Int -> Int)
aaaaa229 i = (Lists.at i [
  1,
  2,
  3])

aaaaa23 :: (Int -> Int)
aaaaa23 i = (Math.add (Lists.at i [
  1,
  2,
  3]) 1)

aaaaa230 :: ([t0] -> t0)
aaaaa230 i = (Lists.at (Math.add 1 2) i)

aaaaa031 :: (t0 -> Int)
aaaaa031 i = (Math.add 1 1)

concat2132 :: ([t0] -> [t0] -> [t0])
concat2132 xs ys = (Lists.concat2 xs ys)

concat2133 :: ([Int] -> [Int])
concat2133 xs = (Lists.concat2 xs [
  1,
  2,
  3])

cons34 :: ([Int] -> [Int])
cons34 xs = (Lists.cons 9 xs)

head35 :: ([t0] -> t0)
head35 xs = (Lists.head xs)

length36 :: ([t0] -> Int)
length36 xs = (Lists.length xs)

length37 :: Int
length37 = (Strings.length "787888")

length38 :: (String -> Int)
length38 xs = (Strings.length xs)

data Person5 = 
  Person5 {
    person5Name :: [Int],
    person5Age :: Int,
    person5Surname :: String,
    person5Int16 :: I.Int16,
    person5Int64 :: I.Int64,
    person5Int641 :: [String]}

addMe39 :: (String -> Bool)
addMe39 x = (Equality.equalString x (Strings.toUpper "123"))

describeNumber40 :: (Int -> String)
describeNumber40 n = (Logic.ifElse (Equality.ltInt32 n 0) "555555" (Logic.ifElse (Equality.equalInt32 n 0) "Zero" "Positive"))
