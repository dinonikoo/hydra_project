module Hydra.Sources.MyStrings where

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

-- | Определение модуля с именем "hydra.string" и списком функций
myStringsModule :: Module
myStringsModule = Module (Namespace "hydra.string") elements [hydraCoreModule] [hydraCoreModule] (Just "Basic string functions")
  where
    elements = [
        el catDef,
        el cat2Def,
        el fromListDef,
        el intercalateDef,
        el isEmptyDef,
        el lengthDef,
        el splitOnDef,
        el toListDef,
        el toLowerDef,
        el toUpperDef
      ]


-- | cat : объединяет список строк в одну строку (concat ["a", "b", "c"] == "abc")
-- Haskell: concat
-- Java: String.join("", list)
-- Python: "".join(list)
catDef :: TElement ([String] -> String)
catDef = definitionInModule myStringsModule "cat" $
  Base.lambda "str" $
    Strings.cat (Base.var "str")


-- | cat2 : соединяет две строки (s1 ++ s2)
-- Haskell: (++)
-- Java: str1 + str2
-- Python: str1 + str2
cat2Def :: TElement (String -> String -> String)
cat2Def = definitionInModule myStringsModule "cat2" $
  Base.lambda "str1" $ Base.lambda "str2" $
    Strings.cat2 (Base.var "str1") (Base.var "str2")


-- | fromList : преобразует список кодов символов (например, [104, 101, 108, 108, 111]) в строку ("hello")
-- Haskell: fromList
-- Java: new String(codes)
-- Python: ''.join(codes)
fromListDef :: TElement ([Int] -> String)
fromListDef = definitionInModule myStringsModule "fromList" $
  Base.lambda "codes" $
    Strings.fromList (Base.var "codes")


-- | intercalate : Вставляет строку-разделитель между элементами списка.
-- Пример: intercalate ", " ["a", "b", "c"] == "a, b, c"
-- Haskell: intercalate
-- Java: String.join
-- Python: sep.join(str)
intercalateDef :: TElement (String -> [String] -> String)
intercalateDef = definitionInModule myStringsModule "intercalate" $
  Base.lambda "sep" $ Base.lambda "str" $
    Strings.intercalate (Base.var "sep") (Base.var "str")


-- | isEmpty : Проверяет, пустая ли строка
-- Haskell: null
-- Java: str.isEmpty()
-- Python: str == ""
isEmptyDef :: TElement (String -> Bool)
isEmptyDef = definitionInModule myStringsModule "isEmpty" $
  Base.lambda "str" $
    Strings.isEmpty (Base.var "str")


-- | length : Возвращает длину строки
-- Haskell: length
-- Java: str.length()
-- Python: len(s)
lengthDef :: TElement (String -> Int)
lengthDef = definitionInModule myStringsModule "length" $
  Base.lambda "s" $
    Strings.length (Base.var "s")

-- | toList : Преобразует строку в список её символов в виде чисел
-- Haskell: toList
-- Java: str.toCharArray()
-- Python: list(s)
toListDef :: TElement (String -> [Int])
toListDef = definitionInModule myStringsModule "toList" $
  Base.lambda "s" $
    Strings.toList (Base.var "s")

-- | toLower : Переводит все буквы строки в нижний регистр
-- Haskell: toLower
-- Java: str.toLowerCase()
-- Python: s.lower()
toLowerDef :: TElement (String -> String)
toLowerDef = definitionInModule myStringsModule "toLower" $
  Base.lambda "s" $
    Strings.toLower (Base.var "s")

-- | toUpper : Переводит все буквы строки в верхний регистр
-- Haskell: toUpper
-- Java: str.toUpperCase()
-- Python: s.upper()
toUpperDef :: TElement (String -> String)
toUpperDef = definitionInModule myStringsModule "toUpper" $
  Base.lambda "s" $
    Strings.toUpper (Base.var "s")


-- | splitOn : Разделяет строку по подстроке-разделителю.
-- Haskell: Data.List.Split.splitOn
-- Java: str.split(sep)
-- Python: s.split(sep)
splitOnDef :: TElement (String -> String -> [String])
splitOnDef = definitionInModule myStringsModule "splitOn" $
  Base.lambda "sep" $ Base.lambda "s" $  
      Strings.splitOn (Base.var "sep") (Base.var "s")
