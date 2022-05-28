{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module AST where

import qualified Data.Text as T
import Data.String (IsString(..))

data Clause = Head :- Body

type Head = Atom

type Body = [ Atom ]

data Atom = Atom Name [ Term ] deriving Eq

data Term = Lit Name | Var Name deriving (Eq, Ord)

type Name = T.Text

instance Show Atom where
  show (Atom name terms) =
    T.unpack (name <> "(" <> T.intercalate "," (map (T.pack . show) terms) <> ")")

instance Show Term where
  show (Lit name) = T.unpack ("\"" <> name <> "\"")
  show (Var name) = T.unpack name

instance IsString Atom where
  fromString name = Atom (T.pack name) []
