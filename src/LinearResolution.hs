module LinearResolution where

import AST

resolve :: [ Atom ] -> Clause -> Maybe [ Atom ]
resolve [] _ = error "Why are you resolving?"
resolve (p : ps) (p' :- qs)
  | p == p' = Just (qs <> ps)
  | otherwise = Nothing

derive :: [ Clause ] -> Atom -> Bool
derive originalClauses query = go [ query ] Nothing originalClauses
  where
  go :: [ Atom ] -> Maybe Clause -> [ Clause ] -> Bool
  go [] _ _ = True
  go goals Nothing (clause : clauses) = go goals (Just clause) clauses
  go goals (Just clause) clauses =
    case resolve goals clause of
      Just goals' | go goals' Nothing originalClauses -> True
      _ -> go goals Nothing clauses
  go _ _ [] = False
