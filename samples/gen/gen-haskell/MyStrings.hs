-- | Basic string functions

module Hydra.String where

import qualified Hydra.Lib.Strings as Strings
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

cat :: ([String] -> String)
cat str = (Strings.cat str)

cat2 :: (String -> String -> String)
cat2 str1 str2 = (Strings.cat2 str1 str2)

fromList :: ([Int] -> String)
fromList codes = (Strings.fromList codes)

intercalate :: (String -> [String] -> String)
intercalate sep str = (Strings.intercalate sep str)

isEmpty :: (String -> Bool)
isEmpty str = (Strings.isEmpty str)

length_ :: (String -> Int)
length_ s = (Strings.length s)

splitOn :: (String -> String -> [String])
splitOn sep s = (Strings.splitOn sep s)

toList_ :: (String -> [Int])
toList_ s = (Strings.toList s)

toLower :: (String -> String)
toLower s = (Strings.toLower s)

toUpper :: (String -> String)
toUpper s = (Strings.toUpper s)
