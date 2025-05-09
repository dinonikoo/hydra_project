module Hydra.Sources.MyLists where

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

-- | Модуль "hydra.lists" содержит базовые функции для работы со списками
myListsModule :: Module
myListsModule = Module (Namespace "hydra.lists") elements [hydraCoreModule] [hydraCoreModule] (Just "Basic list functions")
  where
    elements =  [
        el applyDef,
        el atDef,
        el bindDef,
        el concatDef,
        el concat2Def,
        el consDef,
--        el elemDef,
        el filterDef,
        el foldlDef,
        el headDef,
        el intercalateDef,
        el intersperseDef,
        el lastDef,
        el lengthDef,
        el mapDef,
--        el nubDef,
        el nullDef,
        el pureDef,
        el reverseDef,
        el safeHeadDef,
        el tailDef,
        el zipDef,
        el zipWithDef
      ]


-- |apply: Применяет список функций к списку значений
-- Haskell: (<*>)
-- Java: нет прямого аналога
-- Python: [f(x) for f in fs for x in xs]
applyDef :: TElement ([a -> b] -> [a] -> [b])
applyDef = definitionInModule myListsModule "apply" $
  Base.lambda "fs" $ Base.lambda "xs" $
    Lists.apply (Base.var "fs") (Base.var "xs")

-- | at : Извлекает элемент по индексу
-- Haskell: (!!) 
-- Java: list.get(i) 
-- Python: xs[i]
atDef :: TElement (Int -> [a] -> a)
atDef = definitionInModule myListsModule "at" $
  Base.lambda "i" $ Base.lambda "xs" $
    Lists.at (Base.var "i") (Base.var "xs")

-- | bind : Последовательно применяет функцию, которая возвращает список, и объединяет все результаты.
-- Haskell: >>= 
-- Java: flatMap, 
-- Python: sum([f(x) for x in xs], [])
bindDef :: TElement ([a] -> (a -> [b]) -> [b])
bindDef = definitionInModule myListsModule "bind" $
  Base.lambda "xs" $ Base.lambda "f" $
    Lists.bind (Base.var "xs") (Base.var "f")

-- | concat : Объединяет список списков в один список
-- Haskell: concat 
-- Java: flatMap 
-- Python: sum(xss, [])
concatDef :: TElement ([[a]] -> [a])
concatDef = definitionInModule myListsModule "concat" $
  Base.lambda "xss" $
    Lists.concat (Base.var "xss")

-- | concat2 : Соединение двух списков в один
-- Haskell: (++) 
-- Java: xs.addAll(ys)
-- Python: xs + ys
concat2Def :: TElement ([a] -> [a] -> [a])
concat2Def = definitionInModule myListsModule "concat2" $
  Base.lambda "xs" $ Base.lambda "ys" $
    Lists.concat2 (Base.var "xs") (Base.var "ys")

-- |cons: Добавляет элемент в начало списка
-- Haskell: x:xs
-- Java: list.add(0, x)
-- Python: [x] + xs
consDef :: TElement (a -> [a] -> [a])
consDef = definitionInModule myListsModule "cons" $
  Base.lambda "x" $ Base.lambda "xs" $
    Lists.cons (Base.var "x") (Base.var "xs")

-- |elem: Проверяет, содержится ли элемент в списке (!РУГАЕТСЯ НА ТО, ЧТО ДОЛЖНО БЫТЬ elem :: Eq a => a -> [a] -> Bool, НО Eq a ТОЖЕ НЕ УСТРАИВАЕТ!)

-- Haskell: elem x xs
-- Java: xs.contains(x)
-- Python: x in xss
--elemDef :: TElement (Int -> [Int] -> Bool)
-- elemDef = definitionInModule myListsModule "elem" $
--  Base.lambda "x" $ Base.lambda "xs" $
--      Lists.elem (Base.var "x") (Base.var "xs")

-- | filter : Оставляет только те элементы списка, для которых выполняется предикат
-- Haskell: filter 
-- Java: stream().filter
-- Python: [x for x in xs if p(x)]
filterDef :: TElement ((a -> Bool) -> [a] -> [a])
filterDef = definitionInModule myListsModule "filter" $
  Base.lambda "p" $ Base.lambda "xs" $
    Lists.filter (Base.var "p") (Base.var "xs")

-- | head : Выводит первый элемент списка
-- Haskell: head
-- Java: list.get(0)
-- Python: xs[0]
headDef :: TElement ([a] -> a)
headDef = definitionInModule myListsModule "head" $
  Base.lambda "xs" $
    Lists.head (Base.var "xs")

-- |foldl: Применяет функцию слева направо по списку (свёртка)
-- Haskell: foldl (+) 0 [1,2,3] → 6
-- Java: list.stream().reduce(0, (acc, x) -> acc + x)
-- Python: unctools.reduce(lambda acc, x: acc + x, xs, 0)
foldlDef :: TElement ((b -> a -> b) -> b -> [a] -> b)
foldlDef = definitionInModule myListsModule "foldl" $
  Base.lambda "f" $ Base.lambda "z" $ Base.lambda "xs" $
    Lists.foldl (Base.var "f") (Base.var "z") (Base.var "xs")

-- |intercalate: Объединяет список списков, вставляя разделитель между ними
-- Haskell: intercalate [0] [[1,2],[3,4]] → [1,2,0,3,4]
-- Java: String.join / List.join
-- Python: separator.join(xs)
intercalateDef :: TElement ([a] -> [[a]] -> [a])
intercalateDef = definitionInModule myListsModule "intercalate" $
  Base.lambda "sep" $ Base.lambda "lists" $
    Lists.intercalate (Base.var "sep") (Base.var "lists")

-- |intersperse: Вставляет указанный элемент между всеми элементами списка
-- Haskell: intersperse 0 [1,2,3] → [1,0,2,0,3]
-- Java:  вручную
-- Python: separator.join(xs)
intersperseDef :: TElement (a -> [a] -> [a])
intersperseDef = definitionInModule myListsModule "intersperse" $
  Base.lambda "sep" $ Base.lambda "xs" $
    Lists.intersperse (Base.var "sep") (Base.var "xs")

-- | last : Возвращает последний элемент списка
-- Haskell: last
-- Java: list.get(size-1)
-- Python: xs[-1]
lastDef :: TElement ([a] -> a)
lastDef = definitionInModule myListsModule "last" $
  Base.lambda "xs" $
    Lists.last (Base.var "xs")

-- |length: Возвращает длину списка
-- Haskell: length
-- Java: list.size()
-- Python: len(list)
lengthDef :: TElement ([a] -> Int)
lengthDef = definitionInModule myListsModule "length" $
  Base.lambda "xs" $
    Lists.length (Base.var "xs")

-- |map: Применяет функцию к каждому элементу списка
-- Haskell: map
-- Java: list.stream().map(...)
-- Python: map(f, xs) или [f(x) for x in xs]
mapDef :: TElement ((a -> b) -> [a] -> [b])
mapDef = definitionInModule myListsModule "map" $
  Base.lambda "f" $ Base.lambda "xs" $
    Lists.map (Base.var "f") (Base.var "xs")

-- | nub : Удаляет дубликаты (!РУГАЕТСЯ НА ТО, ЧТО ДОЛЖНО БЫТЬ nub :: Eq a => [a] -> [a], НО Eq a ТОЖЕ НЕ УСТРАИВАЕТ!)
-- Haskell: nub
-- Java: new ArrayList<>(new LinkedHashSet<>(xs))
-- Python: list(dict.fromkeys(xs))
--nubDef :: TElement ([Int] -> [Int])
--nubDef = definitionInModule myListsModule "nub" $
--  Base.lambda "xs" $
--    Lists.nub (Base.var "xs")

-- |null: Проверяет, пустой ли список
-- Haskell: null xs
-- Java: xs.isEmpty()
-- Python: len(xs) == 0 или просто not xs
nullDef :: TElement ([a] -> Bool)
nullDef = definitionInModule myListsModule "null" $
  Base.lambda "xs" $
    Lists.null (Base.var "xs")

-- |pure: Оборачивает элемент в список
-- Haskell: pure x
-- Java: Collections.singletonList(x)
-- Python: [x]
pureDef :: TElement (a -> [a])
pureDef = definitionInModule myListsModule "pure" $
  Base.lambda "x" $
    Lists.pure (Base.var "x")

-- |reverse: Переворачивает список
-- Haskell: reverse
-- Java: Collections.reverse(list)
-- Python: list[::-1]
reverseDef :: TElement ([a] -> [a])
reverseDef = definitionInModule myListsModule "reverse" $
  Base.lambda "xs" $
    Lists.reverse (Base.var "xs")

-- | safeHead : Безопасное извлечение первого элемента
-- Haskell: listToMaybe
-- Python: xs[0] if xs else None
safeHeadDef :: TElement ([a] -> Maybe a)
safeHeadDef = definitionInModule myListsModule "safeHead" $
  Base.lambda "xs" $
    Lists.safeHead (Base.var "xs")

-- | tail : Удаляет первый элемент
-- Haskell: tail
-- Java: subList(1, size)
-- Python: xs[1:]
tailDef :: TElement ([a] -> [a])
tailDef = definitionInModule myListsModule "tail" $
  Base.lambda "xs" $
    Lists.tail (Base.var "xs")

-- |zip: Объединяет два списка в список пар
-- Haskell: zip
-- Java: использовать внешнюю библиотеку(StreamZip)/вручную через цикл
-- Python: zip(xs, ys)
zipDef :: TElement ([a] -> [b] -> [(a, b)])
zipDef = definitionInModule myListsModule "zip" $
  Base.lambda "xs" $ Base.lambda "ys" $
    Lists.zip (Base.var "xs") (Base.var "ys")

-- | zipWith :Объединяет два списка с применением функции к каждой паре элементов
-- Haskell: zipWith
-- Java: stream.zip.map
-- Python: [f(x,y) for x,y in zip(xs, ys)]
zipWithDef :: TElement ((a -> b -> c) -> [a] -> [b] -> [c])
zipWithDef = definitionInModule myListsModule "zipWith" $
  Base.lambda "f" $ Base.lambda "xs" $ Base.lambda "ys" $
    Lists.zipWith (Base.var "f") (Base.var "xs") (Base.var "ys")