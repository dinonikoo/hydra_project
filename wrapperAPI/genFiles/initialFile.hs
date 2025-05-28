import Data.Char

x :: Int
x = 890

y :: Int 
y = 52

str :: String
str = "awful"

eq :: String -> Bool
eq str = str == "awesome"

func :: Int -> Int -> Bool
func x y = ((x >= 10) && (y < 100))

describeNumber :: Int -> String
describeNumber y = (if y < 0 then "negative" else (if (y == 0) then "zero" else "positive"))
