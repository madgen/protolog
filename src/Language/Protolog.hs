module Language.Protolog
  ( module DSL
  , module AST
  , run
  , runWithProvenance
  ) where

import Language.Protolog.LinearResolution (run, runWithProvenance)
import Language.Protolog.AST as AST
import Language.Protolog.DSL as DSL
