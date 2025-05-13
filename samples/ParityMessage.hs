module Hydra.Sources.ParityMessage where

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

-- Подключаем ранее определённые модули
import           Hydra.Sources.Mod (checkDivisibleBy2Def, modModule)

-- Модуль
parityMessageModule :: Module
parityMessageModule = Module (Namespace "hydra.parity_message") elements [modModule] [hydraCoreModule]
  (Just "A module that prints whether the product of two numbers is even or odd")
  where
    elements = [el parityMessageDef]

-- parityMessage :: Int -> Int -> String
parityMessageDef :: TElement (Int -> Int -> String)
parityMessageDef = definitionInModule parityMessageModule "parityMessage" $
  lambda "a" $ lambda "b" $
    let prod = Math.mul (Base.var "a") (Base.var "b")
    in Logic.ifElse
      (Base.apply (ref checkDivisibleBy2Def) prod)
      (Base.string "even")
      (Base.string "odd")