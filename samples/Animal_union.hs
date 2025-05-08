{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Animal_union where

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

animalModule :: Module
animalModule = Module ns elements [hydraCoreModule] [hydraCoreModule] Nothing --параметр для расширений
  where
    ns = Namespace "hydra.example" -- пространство имён из Module
    def = datatype ns  -- Здесь def определено -- определение новой функции def
--datetype используется для создания нового типа данных в определенном пространстве имен
--datatype :: Namespace -> String -> Type -> Element
    core = typeref $ moduleNamespace hydraCoreModule
    testing = typeref ns

--core даёт тебе доступ к типам, уже определённым в основном модуле ядра Hydra. 
--Это полезно, когда ты хочешь использовать типы, которые уже предоставляются 
в рамках Hydra (например, стандартные типы, операции, или абстракции), но не хочешь их заново определять.
--testing даёт тебе доступ к типам, которые ты сам определяешь 
--в своем собственном пространстве имен hydra.example. 
--Это помогает использовать свои типы и элементы внутри текущего модуля или в других местах в коде.

    -- Описание Animal как записи с полями (вид, окрас, вес)
    elements = [
        def "Animal" $
            doc "An abstract animal type" $
--добавление документации
            Types.union [
            "dog" >: Types.record ["name" >: Types.string],
            "cat" >: Types.record ["name" >: Types.string]
            ]
        ]
--Это описание суммарного типа (Union type).
--В данном случае, Animal может быть либо dog, либо cat. 
--Суммарный тип позволяет работать с несколькими типами данных как с одним, 
--что удобно для случаев, когда один тип может быть одним из нескольких возможных.
