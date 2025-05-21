{-# LANGUAGE OverloadedStrings #-}

module Hydra.GenDSL where

import           Hydra.Dsl.Annotations as A
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
import qualified Hydra.Dsl.Lib.Chars as Chars

mainModule :: Module
mainModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just "mainModule generated code"
  where
    ns = Namespace "hydra.testing"
    def = datatype ns
    core = typeref $ moduleNamespace hydraCoreModule
    testing = typeref ns

    elements = [
        def "Person" $ 
            A.doc "This type generated from Java" $ 
            Types.record [
              "name" Types.>: Types.string,
              "age" Types.>: Types.int32
        ],
      el exampleDef,
      el isLowerCaseDef,
      el is_Def,
      el add_Def,
      el arifmDef,
      el subDef,
      el mulDef,
      el divDef,
      el remDef,
      el modDef,
      el lenDef,
      el toLowerDef,
      el eqDef,
      el grDef,
      el greDef,
      el leDef,
      el leeDef,
      el ternDef,
      el listOfListsDef,
      el checkDef
      ]

exampleDef :: TElement Bool
exampleDef = definitionInModule mainModule "example" $
  Equality.equalString (Base.string "hello") (Base.string "hi")

isLowerCaseDef :: TElement Bool
isLowerCaseDef = definitionInModule mainModule "isLowerCase" $
  Chars.isLower(Base.int32 97)

is_Def :: TElement Bool
is_Def = definitionInModule mainModule "is_" $
  Strings.isEmpty (Strings.toLower (Base.string "hello"))

add_Def :: TElement Int
add_Def = definitionInModule mainModule "add_" $
  Math.add (Math.add (Base.int32 1) (Base.int32 1)) (Base.int32 1)

arifmDef :: TElement Int
arifmDef = definitionInModule mainModule "arifm" $
  Math.add (Base.int32 1) (Math.mul (Math.neg (Base.int32 1)) (Base.int32 5))

subDef :: TElement Int
subDef = definitionInModule mainModule "sub" $
  Math.sub (Base.int32 2) (Base.int32 3)

mulDef :: TElement Int
mulDef = definitionInModule mainModule "mul" $
  Math.mul (Base.int32 2) (Base.int32 5)

divDef :: TElement Int
divDef = definitionInModule mainModule "div" $
  Math.div (Base.int32 5) (Base.int32 2)

remDef :: TElement Int
remDef = definitionInModule mainModule "rem" $
  Math.rem (Base.int32 5) (Base.int32 1)

modDef :: TElement Int
modDef = definitionInModule mainModule "mod" $
  Math.mod(Base.int32 1)(Base.int32 2)

lenDef :: TElement Int
lenDef = definitionInModule mainModule "len" $
  Strings.length (Base.string "Hello")

toLowerDef :: TElement Bool
toLowerDef = definitionInModule mainModule "toLower" $
  Equality.equalString (Strings.toLower (Base.string "Hello")) (Base.string "hello")

eqDef :: TElement Bool
eqDef = definitionInModule mainModule "eq" $
  Equality.equalInt32 (Base.int32 5) (Base.int32 6)

grDef :: TElement Bool
grDef = definitionInModule mainModule "gr" $
  Equality.gtInt32 (Base.int32 5) (Base.int32 6)

greDef :: TElement Bool
greDef = definitionInModule mainModule "gre" $
  Equality.gteInt32 (Base.int32 5) (Base.int32 6)

leDef :: TElement Bool
leDef = definitionInModule mainModule "le" $
  Equality.ltInt32 (Base.int32 5) (Base.int32 6)

leeDef :: TElement Bool
leeDef = definitionInModule mainModule "lee" $
  Equality.lteInt32 (Base.int32 5) (Base.int32 6)

ternDef :: TElement String
ternDef = definitionInModule mainModule "tern" $
  Logic.ifElse(Equality.gtInt32 (Base.int32 6) (Base.int32 5)) (Base.string "yes") (Base.string "no")

listOfListsDef :: TElement [Int]
listOfListsDef = definitionInModule mainModule "listOfLists" $
  Base.list [(Base.int32 1), (Base.int32 2), (Base.int32 3)]

checkDef :: TElement (Int -> Bool)
checkDef = definitionInModule mainModule "check" $
  Base.lambda "x" $
    Strings.isEmpty (Logic.ifElse(Equality.gtInt32 (Base.var "x") (Base.int32 5)) (Base.string "yes") (Base.string "no"))
