-- | Basic math functions

module Hydra.Math where

import qualified Hydra.Lib.Math as Math
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

neg :: (Int -> Int)
neg x = (Math.neg x)

add :: (Int -> Int)
add x = (Math.add x 1)

sub :: (Int -> Int)
sub x = (Math.sub x 1)

mul :: (Int -> Int)
mul x = (Math.mul x 1)

div_ :: (Int -> Int)
div_ x = (Math.div x 1)

mod_ :: (Int -> Int)
mod_ x = (Math.mod x 1)

rem_ :: (Int -> Int)
rem_ x = (Math.rem x 1)
