module Untyped where

import Control.Monad
import Data.List

import Common


----------------------------------------------
-- Seccón 2 - Representación de Términos Lambda 
-- Ejercicio 2: Conversión de Términos
----------------------------------------------

convTerms :: LamTerm -> [Name] -> Term 
convTerms (LVar x)    v = case elemIndex x v of
                            Just n  -> Bound n
                            Nothing -> Free x
convTerms (App t1 t2) v = (convTerms t1 v) :@: (convTerms t2 v)
convTerms (Abs x t)   v = Lam $ convTerms t (x:v)

conversion  :: LamTerm -> Term
conversion = \x -> convTerms x []
  
-------------------------------
-- Sección 3 - Evaluación
-------------------------------

shift :: Term -> Int -> Int -> Term
shift (Free x)    c d = Free x
shift (Bound x)   c d | x < c     = Bound x
                      | otherwise = Bound (x + d)
shift (t1 :@: t2) c d = (shift t1 c d) :@: (shift t2 c d)
shift (Lam t)     c d = Lam $ shift t (c+1) d

-------------------------------

subst :: Term -> Term -> Int -> Term
subst (Free k)     t' i = Free k
subst (Bound k)    t' i | k == i    = t'
                        | otherwise = Bound k
subst (t1 :@: t2)  t' i = (subst t1 t' i) :@: (subst t2 t' i)
subst (Lam t)      t' i = Lam $ subst t (shift t' 0 1) (i+1)

-------------------------------

eval :: NameEnv Term -> Term -> Term
eval nvs (Free k)          = case lookup k nvs of
                                Just m  -> eval nvs m
                                Nothing -> Free k
eval nvs (Bound k)         = Bound k
-- E-ABS
eval nvs (Lam t)           = Lam $ eval nvs t
-- E-AppAbs
eval nvs ((Lam t1) :@: t2) = eval nvs $ shift (subst t1 (shift t2 0 1) 0) 0 (-1) 
-- E-App1/App2
eval nvs (t1 :@: t2)       = case eval nvs t1 of
                                Lam t3 -> eval nvs $ Lam t3 :@: t2
                                t3 -> t3 :@: eval nvs t2
