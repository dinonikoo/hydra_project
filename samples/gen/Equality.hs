-- | Equality functions

module Hydra.Equality where

import qualified Hydra.Lib.Equality as Equality
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

equalInt32 :: (Int -> Int -> Bool)
equalInt32 x y = (Equality.equalInt32 x y)

equalString :: (String -> String -> Bool)
equalString a b = (Equality.equalString a b)

gtInt32 :: (Int -> Int -> Bool)
gtInt32 x y = (Equality.gtInt32 x y)

gteInt32 :: (Int -> Int -> Bool)
gteInt32 x y = (Equality.gteInt32 x y)

ltInt32 :: (Int -> Int -> Bool)
ltInt32 x y = (Equality.ltInt32 x y)

lteInt32 :: (Int -> Int -> Bool)
lteInt32 x y = (Equality.lteInt32 x y)
