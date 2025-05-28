import Data.Char

data Person =
  Person {
    name :: [Int],
    age :: Int,
    surname :: String,
    int16 :: Int16,
    int64 :: Int64,
    int641 :: [String]
  }

addMe1 :: String -> Bool
addMe1 x = x ==  map toUpper "123"

addMe2 :: String -> Bool
addMe2 x = x ==  map toUpper "123"



describeNumber3 :: Int -> Integer
describeNumber3 n =
  if n < 0 then 1
  else if n == 0 then 2
  else 3



addMe3 :: String -> Bool
addMe3 x = x == "123"

addMe4 :: String ->String-> Bool
addMe4 x y = x == y

inRange6 :: Int -> Int -> Int -> Bool
inRange6 x a b = (x >= a) && (x <= b)



myTest7 :: Int->Int->Int->Int
myTest7 x y z= (x+y+z) 


myTest8 :: Int->Int->Int->Bool
myTest8 x y z= x/=y

myTest9:: Bool->Bool
myTest9 x = x==True


isEmpty10 :: String  -> Bool
isEmpty10 str = null str 

myConstant11 :: Integer
myConstant11 = 42

isEmpty12 :: String  -> String
isEmpty12 str = map toUpper str   


stringCatList13 :: [String] -> String
stringCatList13 strList = concat strList

toList14 :: String -> [Int]
toList14 codes = map ord codes 

reverse15 :: [Integer] -> [Integer]
reverse15 xs = reverse xs

myList16 :: [Int]
myList16 = [1, 2, 3]

aaa17::Integer
aaa17 = 5 + 5

nullStr18 :: Integer -> String -> Bool
nullStr18 a s = null s && a == 0

equality19 ::String->String ->Bool
equality19 s d =  s==d

equality421 ::String-> Int-> Bool
equality421 a b = a=="7889" && b==1

equality622 :: String->Bool
equality622 a = a==(a++a)

equality623 :: String -> Bool
equality623 a = "456" == ( map toUpper a)


equality111125 ::String -> String ->Bool
equality111125 s d =  s==d

inRange26 :: Int -> Int -> Int -> Bool
inRange26 x a b = (x >= a) && (x <= b)

aaaaa126:: Int->[a]->Int
aaaaa126 i xs  = (xs !! i) + 1

aaaaa27:: [String]
aaaaa27   = ["123"]


aaaaa28:: [Int]
aaaaa28   = [1]


aaaaa229:: Int-> a
aaaaa229 i  = [1,2,3] !! i





aaaaa23:: Int->Int 
aaaaa23 i  =( [1,2,3] !! i )  + 1

aaaaa230:: [a] -> a
aaaaa230 i  =  i !!  (1+2)
aaaaa031:: Int -> Int
aaaaa031 i  =1 + 1

concat2132 :: [Integer] -> [Integer] -> [Integer]
concat2132 xs ys = xs ++ ys

concat2133 :: [Integer] ->  [Integer]
concat2133 xs = xs ++ [1,2,3]

cons34 :: [a] -> [a]
cons34 xs = 9 : xs


head35 :: [a] -> a
head35 xs = head xs


length36 :: [a] -> Int
length36 xs = length xs


length37 :: Int
length37 = length "787888"

length38 :: String -> Int
length38 xs = length xs

data Person5 = 
  Person5 {
    name :: [Int],
    age :: Int,
    surname :: String,
    int16 :: Int16,
    int64 :: Int64,
    int641 :: [String]
  }


addMe39 :: String -> Bool
addMe39 x = x ==  map toUpper "123"

describeNumber40 :: Int -> String
describeNumber40 n =
  if n < 0 then "555555"
  else if n == 0 then "Zero"
  else "Positive"







