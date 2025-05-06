module Hydra.Sources.Mod where

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

modModule :: Module
modModule = Module (Namespace "hydra.test_function") elements [hydraCoreModule] [hydraCoreModule] (Just "A module with a mod function")
  where
    elements = [
        el isEvenDef
      ]

isEvenDef :: TElement (Int -> String)
isEvenDef = definitionInModule modModule "isEven" $
  lambda "x" $
    Logic.ifElse
      (Equality.equalInt32
        (Math.mod (Base.var "x") (Base.int32 2))
        (Base.int32 0))                   -- моё условие: TTerm Bool, так как сравниваем именно типы? это для проверки типов/функций
      (Base.string "even")             -- тогда TTerm String
      (Base.string "odd")           -- иначе TTerm String