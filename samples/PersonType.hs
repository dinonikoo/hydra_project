{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.PersonType where

-- Standard type-level Tier-1 imports
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms       as Terms
import           Hydra.Dsl.Types       as Types
import           Hydra.Sources.Tier0.Core
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Maybe            as Y

import Hydra.Sources.Tier1.Mantle

personModule :: Module
personModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just "A model for unit testing"
  where
    ns = Namespace "hydra.testing"
    def = datatype ns
    core = typeref $ moduleNamespace hydraCoreModule
    testing = typeref ns

    elements = [
      def "Person" $
        doc "Test" $
        record [
          "name" >: string,
          "age" >: string
        ]
      ]
