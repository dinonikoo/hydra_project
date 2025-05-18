import Data.Char
import Data.List.Split
--isUpper1 :: Integer -> Integer
--isUpper1 x = ( negate x )





--describeNumber :: Int -> String describeNumber n =  if n < 0   then 1    else if n == 0      then "Zero"      else "Positive" 


--myTest :: Int->Int->Int->Bool
--myTest x y z= x/=y

--cat :: [String] -> String
--cat str = concat str

--isEmpty :: String  -> Bool
--isEmpty str = null str  -- Используем стандартную функцию null

--myConstant :: Integer
--myConstant = 42

--isEmpty :: String  -> String
--isEmpty str = map toUpper str   


--stringCatList :: [String] -> String
--stringCatList strList = concat strList

--toList :: String -> [Int]
--toList codes = map ord codes 

---reverse :: [Integer] -> [Integer]
--reverse xs = reverse xs

--myList :: [Int]
--myList = [1, 2, 3]

--aaa::Integer
--aaa = 5 + 5

nullStr :: Integer -> String -> Bool
nullStr a s = null s && a == 0
--теперь работает 

--equality ::String->String ->Bool
--equality s d =  s==d
--работает

--equality1 ::Integer->String ->Bool
--equality1 s d =  s==d

equality4 ::String-> Int-> Bool
equality4 a b = a=="7889" && b==1
--работает

equality6 :: String->Bool
equality6 a = a==(a++a)
--работает

equality6 :: String -> Bool
equality6 a = "456" == ( map toUpper a)
--работает

--equality6 :: String->Bool
--equality6 a = a==9
--ошибка


equality1111 ::String -> String ->Bool
equality1111 s d =  s==d
--работает

inRange :: Int -> Int -> Int -> Bool
inRange x a b = (x >= a) && (x <= b)

aaaaa1:: Int->[a]->Int
aaaaa1 i xs  = (xs !! i) + 1

--aaaaa:: [a]
--aaaaa   = ["123"]
--ошибка

aaaaa:: [Int]
aaaaa   = [1]
--ошибка

aaaaa2:: Int-> a
aaaaa2 i  = [1,2,3] !! i



--aaaaa1 ::[String]
--aaaaa1   = ["123", 1]
--ошибка

aaaaa23:: Int->Int 
aaaaa23 i  =( [1,2,3] !! i )  + 1
--работает

aaaaa2:: [a] -> a
aaaaa2 i  =  i !!  (1+2)
--Работает

aaaaa0:: Int -> Int
aaaaa0 i  =1 + 1

concat21 :: [Integer] -> [Integer] -> [Integer]
concat21 xs ys = xs ++ ys

concat21 :: [Integer] ->  [Integer]
concat21 xs = xs ++ [1,2,3]

cons :: [a] -> [a]
cons xs = 9 : xs


head :: [a] -> a
head xs = head xs


length :: [a] -> Int
length xs = length xs
--работает

length :: String -> Int
length xs = length xs
--различается

-- Добавляет элемент в начало списка/работает
{- Оно даже обрабатывает как ошибку   -}

{- dlkfcvnsldkvnslk   -}


