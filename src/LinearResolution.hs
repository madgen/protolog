module LinearResolution where

import AST

type GoalStack = [ [ Atom ] ]

resolve :: GoalStack -> Clause -> Maybe GoalStack
resolve [] _ = error "Why are you resolving?"
resolve ([] : _) _ = error "Why are you resolving?"
resolve ((p : ps) : pss) (q :- qs)
  | p == q = Just (qs : ps : pss)
  | otherwise = Nothing

derive :: [ Clause ] -> Atom -> Bool
derive originalClauses query = go [ [ query ] ] Nothing originalClauses
  where
  go :: GoalStack -> Maybe Clause -> [ Clause ] -> Bool
  go [] _ _ = True
  go ([] : goals) active clauses = go goals active clauses
  go goals Nothing (clause : clauses) = go goals (Just clause) clauses
  go goals (Just clause) clauses =
    case resolve goals clause of
      Just goals' | go goals' Nothing originalClauses -> True
      _ -> go goals Nothing clauses
  go _ _ [] = False
