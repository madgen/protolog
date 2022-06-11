{-# LANGUAGE RecordWildCards #-}
module Language.Protolog.Naming (clause) where

import qualified Data.Text as T

import Language.Protolog.AST

clause :: Int -> Clause -> Clause
clause i (head :- body) = atom i head :- map (literal i) body

literal :: Int -> Literal -> Literal
literal i (Literal polarity a) = Literal polarity (atom i a)

atom :: Int -> Atom -> Atom
atom i Atom{..} = Atom{_terms = map (term i) _terms, ..}

term :: Int -> Term -> Term
term i Fx{..} = Fx{_terms = map (term i) _terms, ..}
term i (Var v) = Var $ T.pack (show i) <> v
