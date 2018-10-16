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
                      | otherwise = Bound $ x + d
shift (t1 :@: t2) c d = (shift t1 c d) :@: (shift t2 c d)
shift (Lam t)     c d = Lam $ shift t (c+1) d


subst :: Term -> Term -> Int -> Term
subst = undefined


eval :: NameEnv Term -> Term -> Term
eval = undefined

