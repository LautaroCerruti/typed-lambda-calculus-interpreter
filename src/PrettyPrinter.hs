module PrettyPrinter
  ( printTerm
  ,     -- pretty printer para terminos
    printType     -- pretty printer para tipos
  )
where

import           Common
import           Text.PrettyPrint.HughesPJ
import           Prelude                 hiding ( (<>) )
-- lista de posibles nombres para variables
vars :: [String]
vars =
  [ c : n
  | n <- "" : map show [(1 :: Integer) ..]
  , c <- ['x', 'y', 'z'] ++ ['a' .. 'w']
  ]

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

-- pretty-printer de términos

pp :: Int -> [String] -> Term -> Doc
pp ii vs (Bound k         ) = text (vs !! (ii - k - 1))
pp _  _  (Free  (Global s)) = text s
pp _  _  Unit = text "unit"
pp _  _  Zero = text "0"

pp ii vs (i :@: c         ) = sep
  [ parensIf (isLam i || isLet i || isAs i || isR i) (pp ii vs i)
  , nest 1 (parensIf (isNVal c) (pp ii vs c))
  ]
pp ii vs (Lam t c) =
  text "\\"
    <> text (vs !! ii)
    <> text ":"
    <> printType t
    <> text ". "
    <> pp (ii + 1) vs c
pp ii vs (Let e1 e2) =
  sep [text "let",
    text (vs !! ii),
    text "=",
    parensIf (isNVal e1) (pp ii vs e1),
    text "in",
    parensIf (isNVal e2) (pp (ii + 1) vs e2)]
pp ii vs (As e1 t1) =
  sep [parensIf (isNVal e1) (pp ii vs e1),
    text "as",
    printType t1]
pp ii vs (Pair e1 e2) =
  parens (sep [parensIf (isNVal e1) (pp ii vs e1),
    text ",",
    parensIf (isNVal e2) (pp ii vs e2)])
pp ii vs (Fst e) =
  sep [text "fst",
    parensIf (isNVal e) (pp ii vs e)]
pp ii vs (Snd e) =
  sep [text "snd",
    parensIf (isNVal e) (pp ii vs e)]
pp ii vs (Suc n) =
  text "suc " <> parensIf (isNVal n) (pp ii vs n)
pp ii vs (Rec e1 e2 e3) =
  sep [text "R",
    parensIf (isNVal e1) (pp ii vs e1),
    parensIf (isNVal e2) (pp ii vs e2),
    parensIf (isNVal e3) (pp ii vs e3)]

isLam :: Term -> Bool
isLam (Lam _ _) = True
isLam _         = False

isAs :: Term -> Bool
isAs (As _ _) = True
isAs _        = False

isApp :: Term -> Bool
isApp (_ :@: _) = True
isApp _         = False

isLet :: Term -> Bool
isLet (Let _ _) = True
isLet _         = False

isR :: Term -> Bool
isR (Rec _ _ _) = True
isR _           = False

isNVal = \t -> isLam t || isApp t || isLet t || isAs t || isR t
-- pretty-printer de tipos
printType :: Type -> Doc
printType EmptyT = text "E"
printType UnitT = text "Unit"
printType NatT = text "Nat"
printType (FunT t1 t2) =
  sep [parensIf (isFun t1) (printType t1), text "->", printType t2]
printType (PairT t1 t2) = 
  parens (sep [printType t1, text ",", printType t2])


isFun :: Type -> Bool
isFun (FunT _ _) = True
isFun _          = False

fv :: Term -> [String]
fv Unit = []
fv Zero = []
fv (Bound _         ) = []
fv (Free  (Global n)) = [n]
fv (t   :@: u       ) = fv t ++ fv u
fv (Lam _   u       ) = fv u
fv (Let t   u       ) = fv t ++ fv u
fv (As e    _       ) = fv e
fv (Pair e1 e2)       = fv e1 ++ fv e2
fv (Fst e)            = fv e
fv (Snd e)            = fv e
fv (Suc n)            = fv n
fv (Rec e1 e2 e3)     = fv e1 ++ fv e2 ++ fv e3

---
printTerm :: Term -> Doc
printTerm t = pp 0 (filter (\v -> not $ elem v (fv t)) vars) t

