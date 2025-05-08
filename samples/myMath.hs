module Hydra.Sources.myMath where

import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms       as Terms
import           Hydra.Dsl.Types       as Types
import           Hydra.Sources.Tier0.Core
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Maybe            as Y
import           Hydra.Sources.Tier1.Mantle
import           Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Lib.Math as Math
import qualified Hydra.Dsl.Module as Module
import qualified Hydra.Dsl.Lib.Logic as Logic
import qualified Hydra.Dsl.Lib.Equality as Equality

-- | Определение модуля с именем "hydra.math" и списком функций
myMathModule :: Module
myMathModule = Module (Namespace "hydra.math") elements [hydraCoreModule] [hydraCoreModule] (Just "Basic math functions")
  where
    elements = [
        el negDef,
        el addDef,
        el subDef,
        el mulDef,
        el divDef,
        el modDef,
        el remDef
      ]



-- | neg :: Int -> Int -возращает обратное число negate(-7) = 7, negate(7) = -7,
-- Haskell: negate x
-- Java: -x
negDef :: TElement (Int -> Int)
negDef = definitionInModule myMathModule "neg" $
  Base.lambda "x" $
    Math.neg (Base.var "x")

-- | add :: Int -> Int -> Int, add x y → x + y
-- Haskell: x + y
-- Java: x + y
addDef :: TElement (Int -> Int -> Int)
addDef = definitionInModule myMathModule "add" $
  Base.lambda "x"$ Math.add (Base.var "x") (Base.int32 1)

-- | sub :: Int -> Int -> Int,  sub x y → x - y
-- Haskell: x - y
-- Java: x - y
subDef :: TElement (Int -> Int -> Int)
subDef = definitionInModule myMathModule "sub" $
  Base.lambda "x" $ Math.sub (Base.var "x") (Base.int32 1)

-- | mul x y → x * y
-- Haskell: x * y
-- Java: x * y
mulDef :: TElement (Int -> Int -> Int)
mulDef = definitionInModule myMathModule "mul" $
  Base.lambda "x"  $ Math.mul (Base.var "x") (Base.int32 1)

-- | div x y → x / y (целочисленное деление)
-- Haskell: div x y
-- Java: x / y (для int деления)
divDef :: TElement (Int -> Int -> Int)
divDef = definitionInModule myMathModule "div" $
  Base.lambda "x" $ Math.div (Base.var "x") (Base.int32 1)

-- | mod x y → x mod y (остаток по модулю)
-- Haskell: mod x y
-- Java: x % y (в Java это больше похоже на rem, не всегда ≥ 0)
modDef :: TElement (Int -> Int -> Int)
modDef = definitionInModule myMathModule "mod" $
  Base.lambda "x" $ Math.mod (Base.var "x") (Base.int32 1)

-- | rem x y → остаток от деления (знак как у делимого) -7%2= -1
-- Haskell: rem x y
-- Java: x % y (так работает Java)
remDef :: TElement (Int -> Int -> Int)
remDef = definitionInModule myMathModule "rem" $
  Base.lambda "x" $ Math.rem (Base.var "x") (Base.int32 1)
