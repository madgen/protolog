module Language.Protolog.Naming (clause) where

import qualified Data.Text as T

import Language.Protolog.AST

clause :: Int -> Clause -> Clause
clause i (head :- body) = atom i head :- map (literal i) body

literal :: Int -> Literal -> Literal
literal i (Literal polarity a) = Literal polarity (atom i a)

atom :: Int -> Atom -> Atom
atom i (Atom name ts) = Atom name $ map (term i) ts

term :: Int -> Term -> Term
term i (Fx name ts) = Fx name (map (term i) ts)
term i (Var v) = Var $ T.pack (show i) <> v
