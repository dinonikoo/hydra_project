module Hydra.Sources.Chars where

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
import Hydra.Dsl.Lib.Chars as Chars


charModule :: Module
charModule = Module (Namespace "hydra.chars") elements [hydraCoreModule] [hydraCoreModule] (Just "Char utilities")
  where
    elements = [
        --el isAlphaNumFunc,
        el isLowerFunc,
        el isUpperFunc  
        el toLowerFunc, 
        el toUpperFunc

      ]

--isAlphaNum - НЕ ГЕНЕРИТСЯ В JAVA

--isAlphaNum :: TTerm Int -> TTerm Bool
--проверка, является ли функция числом или номером 
--В Haskell : isAlphaNum :: Char -> Bool (Модуль: import Data.Char (isAlphaNum)) 
--isAlphaNum 'A'  -- True
--isAlphaNum '1'  -- True
--isAlphaNum '#'  -- False
--Java: Character.isLetterOrDigit(char ch) Описание: возвращает true, если символ — буква (isLetter) или цифра (isDigit).

--isAlphaNumFunc :: TElement (Int -> Int)
--isAlphaNumFunc = definitionInModule charModule "isAlphaNum" $
  --lambda "c" $ Chars.isAlphaNum (Base.var "c")


--isLower :: TTerm Int -> TTerm Bool
-- Haskell	Data.Char.isLower :: Char -> Bool	      isLower 'a' → True
-- Java	    Character.isLowerCase(char c)	          Character.isLowerCase('a') → true

isLowerFunc :: TElement (Int -> Int)
isLowerFunc = definitionInModule charModule "isLower" $
  lambda "c" $ Chars.isLower (Base.var "c")


--isUpper :: TTerm Int -> TTerm Bool
--Haskell    Data.Char.isUpper :: Char -> Bool	   isUpper 'A' → True
--Java    	 Character.isUpperCase(char c)	       Character.isUpperCase('A') → true

isUpperFunc :: TElement (Int -> Int)
isUpperFunc = definitionInModule charModule "isUpper" $
  lambda "c" $ Chars.isUpper (Base.var "c")

--toLower :: TTerm Int -> TTerm Int
--Haskell	   Data.Char.toLower :: Char -> Char	   toLower 'A' → 'a'
--Java	     Character.toLowerCase(char c)	       Character.toLowerCase('A') → 'a'

toLowerFunc :: TElement (Int -> Int)
toLowerFunc = definitionInModule charModule "toLower" $
  lambda "c" $ Chars.toLower (Base.var "c")

--toUpper :: TTerm Int -> TTerm InttoLower :: TElement (Int -> Int)
--Haskell  	Data.Char.toUpper :: Char -> Char	     toUpper 'a' → 'A'
--Java	    Character.toUpperCase(char c)	         Character.toUpperCase('a') → 'A'

toUpperFunc :: TElement (Int -> Int)
toUpperFunc = definitionInModule charModule "toUpper" $
  lambda "c" $ Chars.toUpper (Base.var "c")