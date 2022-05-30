{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module AST where

import qualified Data.Text as T
import Data.String (IsString(..))
import Data.List (intercalate)

data Clause = Head :- Body

type Head = Atom

type Body = [ Atom ]

data Atom = Atom Name [ Term ] deriving Eq

data Term = Fx Name [ Term ] | Var Name deriving (Eq, Ord)

type Name = T.Text

instance {-# OVERLAPPING #-} Show [ Term ] where
  show [] = ""
  show terms = "(" <> intercalate "," (map show terms) <> ")"

instance Show Atom where
  show (Atom name terms) = T.unpack name <> show terms

instance Show Term where
  show (Fx name terms) = T.unpack name <> show terms
  show (Var name) = T.unpack ("?" <> name)

instance IsString Atom where
  fromString name = Atom (T.pack name) []

instance IsString Term where
  fromString ('?' : rest) = Var (T.pack rest)
  fromString str = Fx (T.pack str) []
