module Simplytyped
  ( conversion
  ,    -- conversion a terminos localmente sin nombre
    eval
  ,          -- evaluador
    infer
  ,         -- inferidor de tipos
    quote          -- valores -> terminos
  )
where

import           Data.List
import           Data.Maybe
import           Prelude                 hiding ( (>>=) )
import           Text.PrettyPrint.HughesPJ      ( render )
import           PrettyPrinter
import           Common

-- conversion a términos localmente sin nombres
conversion :: LamTerm -> Term
conversion = conversion' []

conversion' :: [String] -> LamTerm -> Term
conversion' b (LUnit)      = Unit
conversion' b (LVar n    ) = maybe (Free (Global n)) Bound (n `elemIndex` b)
conversion' b (LApp t u  ) = conversion' b t :@: conversion' b u
conversion' b (LAbs n t u) = Lam t (conversion' (n : b) u)
conversion' b (LLet n e1 e2) = Let (conversion' b e1) (conversion' (n : b) e2)
conversion' b (LAs e t) = As (conversion' b e) t
conversion' b (LPair e1 e2) = Pair (conversion' b e1) (conversion' b e2)
conversion' b (LFst e) = Fst (conversion' b e)
conversion' b (LSnd e) = Snd (conversion' b e)

-----------------------
--- eval
-----------------------

sub :: Int -> Term -> Term -> Term
sub _ _ Unit                  = Unit
sub i t (Bound j) | i == j    = t
sub _ _ (Bound j) | otherwise = Bound j
sub _ _ (Free n   )           = Free n
sub i t (u   :@: v)           = sub i t u :@: sub i t v
sub i t (Lam t'  u)           = Lam t' (sub (i + 1) t u)
sub i t (Let e1 e2)           = Let (sub i t e1) (sub (i + 1) t e2)
sub i t (As e t')             = As (sub i t e) t'
sub i t (Pair e1 e2)          = Pair (sub i t e1) (sub i t e2)
sub i t (Fst e)               = Fst (sub i t e)
sub i t (Snd e)               = Snd (sub i t e)

-- evaluador de términos
eval :: NameEnv Value Type -> Term -> Value
eval _ (Bound _             ) = error "variable ligada inesperada en eval"
eval e (Free  n             ) = fst $ fromJust $ lookup n e
eval _ Unit = VUnit
eval _ (Lam      t   u      ) = VLam t u
eval e (Let e1 e2) = eval e (sub 0 (quote (eval e e1)) e2)
eval e (Lam _ u  :@: Lam s v) = eval e (sub 0 (Lam s v) u)
eval e (Lam t u1 :@: u2) = let v2 = eval e u2 in eval e (sub 0 (quote v2) u1)
eval e (As e1 t') = eval e e1
eval e (Pair e1 e2) = VPair (eval e e1) (eval e e2)
eval e (Fst e1) = case eval e e1 of
                    VPair v1 _ -> v1
                    _          -> error "Error de tipo en run-time, verificar type checker"
eval e (Snd e1) = case eval e e1 of
                    VPair _ v2 -> v2
                    _          -> error "Error de tipo en run-time, verificar type checker"
eval e (u        :@: v      ) = case eval e u of
  VLam t u' -> eval e (Lam t u' :@: v)
  _         -> error "Error de tipo en run-time, verificar type checker"


-----------------------
--- quoting
-----------------------

quote :: Value -> Term
quote (VLam t f) = Lam t f
quote VUnit = Unit
quote (VPair e1 e2) = Pair (quote e1) (quote e2)

----------------------
--- type checker
-----------------------

-- type checker
infer :: NameEnv Value Type -> Term -> Either String Type
infer = infer' []

-- definiciones auxiliares
ret :: Type -> Either String Type
ret = Right

err :: String -> Either String Type
err = Left

(>>=)
  :: Either String Type -> (Type -> Either String Type) -> Either String Type
(>>=) v f = either Left f v
-- fcs. de error

matchError :: Type -> Type -> Either String Type
matchError t1 t2 =
  err
    $  "se esperaba "
    ++ render (printType t1)
    ++ ", pero "
    ++ render (printType t2)
    ++ " fue inferido."

notfunError :: Type -> Either String Type
notfunError t1 = err $ render (printType t1) ++ " no puede ser aplicado."

notpairError :: Type -> Either String Type
notpairError t1 = err $ render (printType t1) ++ " no es una tupla."

notfoundError :: Name -> Either String Type
notfoundError n = err $ show n ++ " no está definida."

infer' :: Context -> NameEnv Value Type -> Term -> Either String Type
infer' c _ (Bound i) = ret (c !! i)
infer' _ _ Unit = ret UnitT
infer' _ e (Free  n) = case lookup n e of
  Nothing     -> notfoundError n
  Just (_, t) -> ret t
infer' c e (t :@: u) = infer' c e t >>= \tt -> infer' c e u >>= \tu ->
  case tt of
    FunT t1 t2 -> if (tu == t1) then ret t2 else matchError t1 tu
    _          -> notfunError tt
infer' c e (Lam t u) = infer' (t : c) e u >>= \tu -> ret $ FunT t tu
infer' c e (Let e1 e2) = infer' c e e1 >>= \t1 -> infer' (t1 : c) e e2
infer' c e (As e1 t') = infer' c e e1 >>= \te -> if (te == t') then ret te else matchError t' te
infer' c e (Pair e1 e2) = infer' c e e1 >>= \t1 -> infer' c e e2 >>= \t2 -> ret (PairT t1 t2)
infer' c e (Fst e1) = infer' c e e1 >>= \tt -> 
  case tt of 
    PairT t1 t2 -> ret t1
    _           -> notpairError tt
infer' c e (Snd e1) = infer' c e e1 >>= \tt -> 
  case tt of 
    PairT t1 t2 -> ret t2
    _           -> notpairError tt