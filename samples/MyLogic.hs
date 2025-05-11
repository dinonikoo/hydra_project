module Hydra.Sources.MyLogic where

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

-- | Определение модуля с именем "hydra.logic" и списком функций
myLogicModule :: Module
myLogicModule = Module (Namespace "hydra.logic") elements [hydraCoreModule] [hydraCoreModule] (Just "Basic logic functions")
  where
    elements = [
        el andDef,
        el ifElseDef,
        el notDef,
        el orDef
      ]

andDef :: TElement (Int -> Bool)
andDef = definitionInModule myLogicModule "and" $
  Base.lambda "x" $
    Logic.and (Equality.equalInt32
        (Base.var "x")
        (Base.int32 0))
        (Equality.equalInt32
        (Base.var "x")
        (Base.int32 1))

ifElseDef :: TElement (Int -> Bool)
ifElseDef = definitionInModule myLogicModule "ifElse" $
  Base.lambda "x"$ Logic.ifElse
      (Equality.equalInt32
        (Math.mod (Base.var "x") (Base.int32 2))
        (Base.int32 0))
      (Base.boolean True)
      (Base.boolean False)

notDef :: TElement (Int -> Bool)
notDef = definitionInModule myLogicModule "not" $
  Base.lambda "x" $ Logic.not (Equality.equalInt32
        (Base.var "x")
        (Base.int32 0))

orDef :: TElement (Int -> Bool)
orDef = definitionInModule myLogicModule "or" $
  Base.lambda "x"  $ Logic.or (Equality.equalInt32
        (Base.var "x")
        (Base.int32 0))
        (Equality.equalInt32
        (Base.var "x")
        (Base.int32 1))
