-- | Basic logic functions

module Hydra.Logic where

import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Math as Math
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

and_ :: (Int -> Bool)
and_ x = (Logic.and (Equality.equalInt32 x 0) (Equality.equalInt32 x 1))

ifElse :: (Int -> Bool)
ifElse x = (Logic.ifElse (Equality.equalInt32 (Math.mod x 2) 0) True False)

not_ :: (Int -> Bool)
not_ x = (Logic.not (Equality.equalInt32 x 0))

or_ :: (Int -> Bool)
or_ x = (Logic.or (Equality.equalInt32 x 0) (Equality.equalInt32 x 1))
