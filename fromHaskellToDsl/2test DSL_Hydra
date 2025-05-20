{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.PersonType where




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
      def "Person" $
        Types.record [
            "name" Types.>: Types.list(Types.int32),
            "age" Types.>: Types.int32,
            "surname" Types.>: Types.string,
            "int16" Types.>: Types.int16,
            "int64" Types.>: Types.int64,
            "int641" Types.>: Types.list(Types.string)
        ],
        el addMe1Def,
        el addMe2Def,
        el describeNumber3Def,
        el addMe3Def,
        el addMe4Def,
        el inRange6Def,
        el myTest7Def,
        el myTest8Def,
        el myTest9Def,
        el isEmpty10Def,
        el myConstant11Def,
        el isEmpty12Def,
        el stringCatList13Def,
        el toList14Def,
        el reverse15Def,
        el myList16Def,
        el aaa17Def,
        el nullStr18Def,
        el equality19Def,
        el equality421Def,
        el equality622Def,
        el equality623Def,
        el equality111125Def,
        el inRange26Def,
        el aaaaa126Def,
        el aaaaa27Def,
        el aaaaa28Def,
        el aaaaa229Def,
        el aaaaa23Def,
        el aaaaa230Def,
        el aaaaa031Def,
        el concat2132Def,
        el concat2133Def,
        el cons34Def,
        el head35Def,
        el length36Def,
        el length37Def,
        el length38Def,
      def "Person5" $
        Types.record [
            "name" Types.>: Types.list(Types.int32),
            "age" Types.>: Types.int32,
            "surname" Types.>: Types.string,
            "int16" Types.>: Types.int16,
            "int64" Types.>: Types.int64,
            "int641" Types.>: Types.list(Types.string)
        ],
        el addMe39Def,
        el describeNumber40Def]

addMe1Def :: TElement (String -> Bool)
addMe1Def = definitionInModule mainModule "addMe1" $
  Base.lambda "x" $ Equality.equalString ((Base.var "x")) (Strings.toUpper ((Base.string "123")))

addMe2Def :: TElement (String -> Bool)
addMe2Def = definitionInModule mainModule "addMe2" $
  Base.lambda "x" $ Equality.equalString ((Base.var "x")) (Strings.toUpper ((Base.string "123")))

describeNumber3Def :: TElement (Int -> Int)
describeNumber3Def = definitionInModule mainModule "describeNumber3" $
  Base.lambda "n" $ Logic.ifElse (Equality.ltInt32 ((Base.var "n")) ((Base.int32 0))) ((Base.int32 1)) (Logic.ifElse (Equality.equalInt32 ((Base.var "n")) ((Base.int32 0))) ((Base.int32 2)) ((Base.int32 3)))

addMe3Def :: TElement (String -> Bool)
addMe3Def = definitionInModule mainModule "addMe3" $
  Base.lambda "x" $ Equality.equalString ((Base.var "x")) ((Base.string "123"))

addMe4Def :: TElement (String -> String -> Bool)
addMe4Def = definitionInModule mainModule "addMe4" $
  Base.lambda "x" $   Base.lambda "y" $ Equality.equalString ((Base.var "x")) ((Base.var "y"))

inRange6Def :: TElement (Int -> Int -> Int -> Bool)
inRange6Def = definitionInModule mainModule "inRange6" $
  Base.lambda "x" $   Base.lambda "a" $   Base.lambda "b" $ Logic.and (Equality.gteInt32 ((Base.var "x")) ((Base.var "a"))) (Equality.lteInt32 ((Base.var "x")) ((Base.var "b")))

myTest7Def :: TElement (Int -> Int -> Int -> Int)
myTest7Def = definitionInModule mainModule "myTest7" $
  Base.lambda "x" $   Base.lambda "y" $   Base.lambda "z" $ Math.add (Math.add ((Base.var "x")) ((Base.var "y"))) ((Base.var "z"))

myTest8Def :: TElement (Int -> Int -> Int -> Bool)
myTest8Def = definitionInModule mainModule "myTest8" $
  Base.lambda "x" $   Base.lambda "y" $   Base.lambda "z" $ Logic.not (Equality.equalInt32 ((Base.var "x")) ((Base.var "y")))

myTest9Def :: TElement (Bool -> Bool)
myTest9Def = definitionInModule mainModule "myTest9" $
  Base.lambda "x" $ Equality.equalBoolean ((Base.var "x")) (Base.true)

isEmpty10Def :: TElement (String -> Bool)
isEmpty10Def = definitionInModule mainModule "isEmpty10" $
  Base.lambda "str" $ Strings.isEmpty ((Base.var "str"))

myConstant11Def :: TElement (Int)
myConstant11Def = definitionInModule mainModule "myConstant11" $
  (Base.int32 42)

isEmpty12Def :: TElement (String -> String)
isEmpty12Def = definitionInModule mainModule "isEmpty12" $
  Base.lambda "str" $ Strings.toUpper ((Base.var "str"))

stringCatList13Def :: TElement ([String] -> String)
stringCatList13Def = definitionInModule mainModule "stringCatList13" $
  Base.lambda "strList" $ Strings.cat ((Base.var "strList"))

toList14Def :: TElement (String -> [Int])
toList14Def = definitionInModule mainModule "toList14" $
  Base.lambda "codes" $ Strings.toList ((Base.var "codes"))

reverse15Def :: TElement ([Int] -> [Int])
reverse15Def = definitionInModule mainModule "reverse15" $
  Base.lambda "xs" $ Lists.reverse ((Base.var "xs"))

myList16Def :: TElement ([Int])
myList16Def = definitionInModule mainModule "myList16" $
  Base.list [(Base.int32 1), (Base.int32 2), (Base.int32 3)]

aaa17Def :: TElement (Int)
aaa17Def = definitionInModule mainModule "aaa17" $
  Math.add ((Base.int32 5)) ((Base.int32 5))

nullStr18Def :: TElement (Int -> String -> Bool)
nullStr18Def = definitionInModule mainModule "nullStr18" $
  Base.lambda "a" $   Base.lambda "s" $ Logic.and (Strings.isEmpty ((Base.var "s"))) (Equality.equalInt32 ((Base.var "a")) ((Base.int32 0)))

equality19Def :: TElement (String -> String -> Bool)
equality19Def = definitionInModule mainModule "equality19" $
  Base.lambda "s" $   Base.lambda "d" $ Equality.equalString ((Base.var "s")) ((Base.var "d"))

equality421Def :: TElement (String -> Int -> Bool)
equality421Def = definitionInModule mainModule "equality421" $
  Base.lambda "a" $   Base.lambda "b" $ Logic.and (Equality.equalString ((Base.var "a")) ((Base.string "7889"))) (Equality.equalInt32 ((Base.var "b")) ((Base.int32 1)))

equality622Def :: TElement (String -> Bool)
equality622Def = definitionInModule mainModule "equality622" $
  Base.lambda "a" $ Equality.equalString ((Base.var "a")) (Strings.cat2 ((Base.var "a")) ((Base.var "a")))

equality623Def :: TElement (String -> Bool)
equality623Def = definitionInModule mainModule "equality623" $
  Base.lambda "a" $ Equality.equalString ((Base.string "456")) (Strings.toUpper ((Base.var "a")))

equality111125Def :: TElement (String -> String -> Bool)
equality111125Def = definitionInModule mainModule "equality111125" $
  Base.lambda "s" $   Base.lambda "d" $ Equality.equalString ((Base.var "s")) ((Base.var "d"))

inRange26Def :: TElement (Int -> Int -> Int -> Bool)
inRange26Def = definitionInModule mainModule "inRange26" $
  Base.lambda "x" $   Base.lambda "a" $   Base.lambda "b" $ Logic.and (Equality.gteInt32 ((Base.var "x")) ((Base.var "a"))) (Equality.lteInt32 ((Base.var "x")) ((Base.var "b")))

aaaaa126Def :: TElement (Int -> [a] -> Int)
aaaaa126Def = definitionInModule mainModule "aaaaa126" $
  Base.lambda "i" $   Base.lambda "xs" $ Math.add (Lists.at ((Base.var "i")) ((Base.var "xs"))) ((Base.int32 1))

aaaaa27Def :: TElement ([String])
aaaaa27Def = definitionInModule mainModule "aaaaa27" $
  Base.list [(Base.string "123")]

aaaaa28Def :: TElement ([Int])
aaaaa28Def = definitionInModule mainModule "aaaaa28" $
  Base.list [(Base.int32 1)]

aaaaa229Def :: TElement (Int -> a)
aaaaa229Def = definitionInModule mainModule "aaaaa229" $
  Base.lambda "i" $ Lists.at ((Base.var "i")) (  Base.list [(Base.int32 1), (Base.int32 2), (Base.int32 3)])

aaaaa23Def :: TElement (Int -> Int)
aaaaa23Def = definitionInModule mainModule "aaaaa23" $
  Base.lambda "i" $ Math.add (Lists.at ((Base.var "i")) (  Base.list [(Base.int32 1), (Base.int32 2), (Base.int32 3)])) ((Base.int32 1))

aaaaa230Def :: TElement ([a] -> a)
aaaaa230Def = definitionInModule mainModule "aaaaa230" $
  Base.lambda "i" $ Lists.at (Math.add ((Base.int32 1)) ((Base.int32 2))) ((Base.var "i"))

aaaaa031Def :: TElement (Int -> Int)
aaaaa031Def = definitionInModule mainModule "aaaaa031" $
  Base.lambda "i" $ Math.add ((Base.int32 1)) ((Base.int32 1))

concat2132Def :: TElement ([Int] -> [Int] -> [Int])
concat2132Def = definitionInModule mainModule "concat2132" $
  Base.lambda "xs" $   Base.lambda "ys" $ Lists.concat2 ((Base.var "xs")) ((Base.var "ys"))

concat2133Def :: TElement ([Int] -> [Int])
concat2133Def = definitionInModule mainModule "concat2133" $
  Base.lambda "xs" $ Lists.concat2 ((Base.var "xs")) (  Base.list [(Base.int32 1), (Base.int32 2), (Base.int32 3)])

cons34Def :: TElement ([a] -> [a])
cons34Def = definitionInModule mainModule "cons34" $
  Base.lambda "xs" $ Lists.cons ((Base.int32 9)) ((Base.var "xs"))

head35Def :: TElement ([a] -> a)
head35Def = definitionInModule mainModule "head35" $
  Base.lambda "xs" $ Lists.head ((Base.var "xs"))

length36Def :: TElement ([a] -> Int)
length36Def = definitionInModule mainModule "length36" $
  Base.lambda "xs" $ Lists.length ((Base.var "xs"))

length37Def :: TElement (Int)
length37Def = definitionInModule mainModule "length37" $
  Strings.length ((Base.string "787888"))

length38Def :: TElement (String -> Int)
length38Def = definitionInModule mainModule "length38" $
  Base.lambda "xs" $ Strings.length ((Base.var "xs"))

addMe39Def :: TElement (String -> Bool)
addMe39Def = definitionInModule mainModule "addMe39" $
  Base.lambda "x" $ Equality.equalString ((Base.var "x")) (Strings.toUpper ((Base.string "123")))

describeNumber40Def :: TElement (Int -> String)
describeNumber40Def = definitionInModule mainModule "describeNumber40" $
  Base.lambda "n" $ Logic.ifElse (Equality.ltInt32 ((Base.var "n")) ((Base.int32 0))) ((Base.string "555555")) (Logic.ifElse (Equality.equalInt32 ((Base.var "n")) ((Base.int32 0))) ((Base.string "Zero")) ((Base.string "Positive")))
