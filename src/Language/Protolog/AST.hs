{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Protolog.AST where

import qualified Data.Text as T
import Data.String (IsString(..))
import Data.List (intercalate)

infix 5 :-

data Clause = Head :- Body deriving Eq

type Head = Atom

type Body = [ Literal ]

data Literal = Literal Polarity Atom deriving Eq

data Polarity = Positive | Negative deriving Eq

data Atom = Atom Name [ Term ] deriving Eq

data Term = Fx Name [ Term ] | Var Name deriving (Eq, Ord)

type Name = T.Text

instance Show Clause where
  show (head :- body) = show head <> " :- " <> intercalate ", " (map show body)

instance Show Literal where
  show (Literal Positive atom) = show atom
  show (Literal Negative atom) = "neg " <> show atom

instance {-# OVERLAPPING #-} Show [ Term ] where
  show [] = ""
  show terms = "(" <> intercalate "," (map show terms) <> ")"

instance Show Atom where
  show (Atom name terms) = T.unpack name <> show terms

instance Show Term where
  show (Fx name terms) = T.unpack name <> show terms
  show (Var name) = T.unpack ("?" <> name)

instance IsString Term where
  fromString ('?' : rest) = Var (T.pack rest)
  fromString str = Fx (T.pack str) []
