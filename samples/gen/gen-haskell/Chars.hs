-- | Char utilities

module Hydra.Chars where

import qualified Hydra.Lib.Chars as Chars
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

isLower :: (Int -> Bool)
isLower c = (Chars.isLower c)

isUpper :: (Int -> Bool)
isUpper c = (Chars.isUpper c)

toLower :: (Int -> Int)
toLower c = (Chars.toLower c)

toUpper :: (Int -> Int)
toUpper c = (Chars.toUpper c)
