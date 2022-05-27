{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module AST where

import qualified Data.Text as T
import Data.String (IsString)

data Clause = Head :- Body

type Head = Atom

type Body = [ Atom ]

newtype Atom = Atom Name deriving (Eq, IsString)

type Name = T.Text

instance Show Atom where
  show (Atom name) = T.unpack name
