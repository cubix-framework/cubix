{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Derive.Equality
-- Copyright   :  (c) 2011 Patrick Bahr
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Automatically derive instances of @EqHF@.
--
--------------------------------------------------------------------------------
module Data.Comp.Multi.Derive.Equality
    (
     EqHF(..),
     KEq(..),
     makeEqHF
    ) where

import Data.Comp.Derive.Utils
import Data.Comp.Multi.Equality
import Language.Haskell.TH hiding (Cxt, match)

{-| Derive an instance of 'EqHF' for a type constructor of any higher-order
  kind taking at least two arguments. -}
makeEqHF :: Name -> Q [Dec]
makeEqHF fname = do
  Just (DataInfo _cxt name args constrs _deriving) <- abstractNewtypeQ $ reify fname
  let args' = init args
      argNames = map (VarT . tyVarBndrName) (init args')
      ftyp = VarT . tyVarBndrName $ last args'
      complType = foldl AppT (ConT name) argNames
      preCond = map (mkClassP ''Eq . (: [])) argNames
      classType = AppT (ConT ''EqHF) complType
  constrs' <- mapM normalConExp constrs
  eqFDecl <- funD 'eqHF  (eqFClauses ftyp constrs constrs')
  return [mkInstanceD preCond classType [eqFDecl]]
      where eqFClauses ftyp constrs constrs' = map (genEqClause ftyp) constrs'
                                   ++ defEqClause constrs
            defEqClause constrs
                | length constrs  < 2 = []
                | otherwise = [clause [wildP,wildP] (normalB [|False|]) []]
            genEqClause ftyp (constr, argts, gadtTy) = do
              let n = length argts
              varNs <- newNames n "x"
              varNs' <- newNames n "y"
              let pat = ConP constr $ map VarP varNs
                  pat' = ConP constr $ map VarP varNs'
                  vars = map VarE varNs
                  vars' = map VarE varNs'
                  mkEq ty x y = let (x',y') = (return x,return y)
                                in if containsType ty (getBinaryFArg ftyp gadtTy)
                                   then [| $x' `keq` $y'|]
                                   else [| $x' == $y'|]
                  eqs = listE $ zipWith3 mkEq argts vars vars'
              body <- if n == 0
                      then [|True|]
                      else [|and $eqs|]
              return $ Clause [pat, pat'] (NormalB body) []
