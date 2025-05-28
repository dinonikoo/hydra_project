-- This file was auto-generated from Haskell

module Hydra.GenDSL where

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
import qualified Hydra.Dsl.Lib.Strings as Strings
import qualified Hydra.Dsl.Lib.Lists as Lists


mainModule :: Module
mainModule = Module (Namespace "hydra.test") elements [hydraCoreModule] [hydraCoreModule] (Just "Test functions")
  where
    ns = Namespace "hydra.test"
    def = datatype ns
    elements = [
        el xDef,
        el yDef,
        el strDef,
        el eqDef,
        el funcDef,
        el describeNumberDef]

xDef :: TElement (Int)
xDef = definitionInModule mainModule "x" $
  (Base.int32 890)

yDef :: TElement (Int)
yDef = definitionInModule mainModule "y" $
  (Base.int32 52)

strDef :: TElement (String)
strDef = definitionInModule mainModule "str" $
  (Base.string "awful")

eqDef :: TElement (String -> Bool)
eqDef = definitionInModule mainModule "eq" $
  Base.lambda "str" $ Equality.equalString ((Base.var "str")) ((Base.string "awesome"))

funcDef :: TElement (Int -> Int -> Bool)
funcDef = definitionInModule mainModule "func" $
  Base.lambda "x" $   Base.lambda "y" $ Logic.and (Equality.gteInt32 ((Base.var "x")) ((Base.int32 10))) (Equality.ltInt32 ((Base.var "y")) ((Base.int32 100)))

describeNumberDef :: TElement (Int -> String)
describeNumberDef = definitionInModule mainModule "describeNumber" $
  Base.lambda "y" $ Logic.ifElse (Equality.ltInt32 ((Base.var "y")) ((Base.int32 0))) ((Base.string "negative")) (Logic.ifElse (Equality.equalInt32 ((Base.var "y")) ((Base.int32 0))) ((Base.string "zero")) ((Base.string "positive")))
