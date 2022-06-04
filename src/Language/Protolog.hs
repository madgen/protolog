module Language.Protolog
  ( module DSL
  , run
  , runWithProvenance
  ) where

import Language.Protolog.LinearResolution (run, runWithProvenance)
import qualified Language.Protolog.DSL as DSL
