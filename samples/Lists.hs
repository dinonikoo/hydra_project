module Hydra.Sources.Lists where

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
import qualified Hydra.Dsl.Lib.Lists as Lists
import qualified Hydra.Dsl.Lib.Strings as Strings

--Функции с массивами в JAva и Python вроде ещё не добавлены

listModule :: Module
listModule = Module (Namespace "hydra.lists") elements [hydraCoreModule] [hydraCoreModule] (Just "A module with lists")
  where
    elements = [ el exampleListDef, el stringListDef]

exampleListDef :: TElement [Int]
exampleListDef = definitionInModule listModule "exampleList" $
  Base.list [Base.int32 1, Base.int32 2, Base.int32 3]

stringListDef :: TElement [String]
stringListDef = definitionInModule listModule "stringList" $
  Base.list [Base.string "hello", Base.string "hydra", Base.string "dsl"]


