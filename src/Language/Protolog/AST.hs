{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Protolog.AST where

import qualified Data.Text as T
import Data.String (IsString(..))
import Data.List (intercalate, nub)

infix 5 :-

data Clause = Head :- Body deriving Eq

type Head = Atom

type Body = [ Literal ]

data Literal = Literal Polarity Atom deriving Eq

data Polarity = Positive | Negative deriving Eq

data Atom = Atom
  { _canonicalName :: Name
  , _printName :: Name
  , _terms :: [ Term ]
  } deriving Eq

data Term =
    Fx
      { _canonicalName :: Name
      , _printName :: Name
      , _terms :: [ Term ]
      }
  | Var Name deriving (Eq, Ord)

type Name = T.Text

instance {-# OVERLAPPING #-} Show [ Clause ] where
  show clauses = intercalate "\n" (map show clauses)

instance Show Clause where
  show (head :- []) = show head <> "."
  show (head :- body) =
    show head <> " |- " <> intercalate " /\\ " (map show body) <> "."

instance Show Literal where
  show (Literal Positive atom) = show atom
  show (Literal Negative atom) = "neg " <> show atom

instance {-# OVERLAPPING #-} Show [ Term ] where
  show [] = ""
  show terms = "(" <> intercalate ", " (map show terms) <> ")"

instance Show Atom where
  show Atom{..} = T.unpack _printName <> show _terms

instance Show Term where
  show Fx{..} = T.unpack _printName <> show _terms
  show (Var name) = T.unpack ("?" <> name)

instance IsString Term where
  fromString ('?' : rest) = Var (T.pack rest)
  fromString str = Fx {_canonicalName = t, _printName = t, _terms = []}
    where
    t = T.pack str

class HasVars a where
  vars :: a -> [ Term ]

instance HasVars Literal where
  vars (Literal _ atom) = vars atom

instance HasVars Atom where
  vars Atom{_terms} = nub $ concatMap vars _terms

instance HasVars Term where
  vars t@(Var x) = [ t ]
  vars t@Fx{_terms} = nub $ concatMap vars _terms
