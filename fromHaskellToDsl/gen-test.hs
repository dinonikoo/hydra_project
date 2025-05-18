-- This file was auto-generated from Haskell

module Hydra.Sources.MyMath where

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


myModuleTest :: Module
myModuleTest = Module (Namespace "hydra.test") elements [hydraCoreModule] [hydraCoreModule] (Just "Test functions")
  where
    elements = [
      el nullStrDef,
      el equality4Def,
      el equality6Def,
      el equality6Def,
      el equality1111Def,
      el inRangeDef,
      el aaaaa1Def,
      el aaaaaDef,
      el aaaaa2Def,
      el aaaaa23Def,
      el aaaaa2Def,
      el aaaaa0Def,
      el concat21Def,
      el concat21Def,
      el consDef,
      el headDef,
      el lengthDef,
      el lengthDef]

nullStrDef :: TElement (Int -> String -> Bool)
nullStrDef = definitionInModule myModuleTest "nullStr" $
  Base.lambda "a" $   Base.lambda "s" $ Logic.and (Strings.isEmpty ((Base.var "s"))) (Equality.equalInt32 ((Base.var "a")) ((Base.int32 0)))

equality4Def :: TElement (String -> Int -> Bool)
equality4Def = definitionInModule myModuleTest "equality4" $
  Base.lambda "a" $   Base.lambda "b" $ Logic.and (Equality.equalString ((Base.var "a")) ((Base.string "7889"))) (Equality.equalInt32 ((Base.var "b")) ((Base.int32 1)))

equality6Def :: TElement (String -> Bool)
equality6Def = definitionInModule myModuleTest "equality6" $
  Base.lambda "a" $ Equality.equalString ((Base.var "a")) (Strings.cat2 ((Base.var "a")) ((Base.var "a")))

equality6Def :: TElement (String -> Bool)
equality6Def = definitionInModule myModuleTest "equality6" $
  Base.lambda "a" $ Equality.equalString ((Base.string "456")) (Strings.toUpper ((Base.var "a")))

equality1111Def :: TElement (String -> String -> Bool)
equality1111Def = definitionInModule myModuleTest "equality1111" $
  Base.lambda "s" $   Base.lambda "d" $ Equality.equalString ((Base.var "s")) ((Base.var "d"))

inRangeDef :: TElement (Int -> Int -> Int -> Bool)
inRangeDef = definitionInModule myModuleTest "inRange" $
  Base.lambda "x" $   Base.lambda "a" $   Base.lambda "b" $ Logic.and (Equality.gteInt32 ((Base.var "x")) ((Base.var "a"))) (Equality.lteInt32 ((Base.var "x")) ((Base.var "b")))

aaaaa1Def :: TElement (Int -> [a] -> Int)
aaaaa1Def = definitionInModule myModuleTest "aaaaa1" $
  Base.lambda "i" $   Base.lambda "xs" $ Math.add (Lists.at ((Base.var "i")) ((Base.var "xs"))) ((Base.int32 1))

aaaaaDef :: TElement ([Int])
aaaaaDef = definitionInModule myModuleTest "aaaaa" $
  Base.list [(Base.int32 1)]

aaaaa2Def :: TElement (Int -> a)
aaaaa2Def = definitionInModule myModuleTest "aaaaa2" $
  Base.lambda "i" $ Lists.at ((Base.var "i")) (  Base.list [(Base.int32 1), (Base.int32 2), (Base.int32 3)])

aaaaa23Def :: TElement (Int -> Int)
aaaaa23Def = definitionInModule myModuleTest "aaaaa23" $
  Base.lambda "i" $ Math.add (Lists.at ((Base.var "i")) (  Base.list [(Base.int32 1), (Base.int32 2), (Base.int32 3)])) ((Base.int32 1))

aaaaa2Def :: TElement ([a] -> a)
aaaaa2Def = definitionInModule myModuleTest "aaaaa2" $
  Base.lambda "i" $ Lists.at (Math.add ((Base.int32 1)) ((Base.int32 2))) ((Base.var "i"))

aaaaa0Def :: TElement (Int -> Int)
aaaaa0Def = definitionInModule myModuleTest "aaaaa0" $
  Base.lambda "i" $ Math.add ((Base.int32 1)) ((Base.int32 1))

concat21Def :: TElement ([Int] -> [Int] -> [Int])
concat21Def = definitionInModule myModuleTest "concat21" $
  Base.lambda "xs" $   Base.lambda "ys" $ Lists.concat2 ((Base.var "xs")) ((Base.var "ys"))

concat21Def :: TElement ([Int] -> [Int])
concat21Def = definitionInModule myModuleTest "concat21" $
  Base.lambda "xs" $ Strings.cat2 ((Base.var "xs")) (  Base.list [(Base.int32 1), (Base.int32 2), (Base.int32 3)])

consDef :: TElement ([a] -> [a])
consDef = definitionInModule myModuleTest "cons" $
  Base.lambda "xs" $ Lists.cons ((Base.int32 9)) ((Base.var "xs"))

headDef :: TElement ([a] -> a)
headDef = definitionInModule myModuleTest "head" $
  Base.lambda "xs" $ Lists.head ((Base.var "xs"))

lengthDef :: TElement ([a] -> Int)
lengthDef = definitionInModule myModuleTest "length" $
  Base.lambda "xs" $ Lists.length ((Base.var "xs"))

lengthDef :: TElement (String -> Int)
lengthDef = definitionInModule myModuleTest "length" $
  Base.lambda "xs" $ Strings.length ((Base.var "xs"))
