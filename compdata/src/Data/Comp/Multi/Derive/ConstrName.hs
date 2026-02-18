{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Derive.ConstrName
-- Copyright   :  (c) 2026 James Koppel
-- License     :  BSD3
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Automatically derive instances of @ConstrNameHF@.
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Derive.ConstrName
    (
     ConstrNameHF(..),
     makeConstrNameHF
    ) where

import Data.Comp.Derive.Utils
import Data.Comp.Multi.Kinds (Fragment)
import Language.Haskell.TH

{-| Get the constructor name of a higher-order functor value. -}
class ConstrNameHF (f :: Fragment) where
    constrNameHF :: f e l -> String

{-| Derive an instance of 'ConstrNameHF' for a type constructor of any higher-order
  kind taking at least two arguments. -}
makeConstrNameHF :: Name -> Q [Dec]
makeConstrNameHF fname = do
  Just (DataInfo _cxt name args constrs _deriving) <- abstractNewtypeQ $ reify fname
  let args' = init args
      argNames = map (VarT . tyVarBndrName) (init args')
      complType = foldl AppT (ConT name) argNames
      classType = AppT (ConT ''ConstrNameHF) complType
  constrs' <- mapM normalConExp constrs
  methDecl <- funD 'constrNameHF (map genClause constrs')
  return [mkInstanceD [] classType [methDecl]]
    where
      genClause (constr, cArgs, _ty) = do
        let n = length cArgs
            pat = ConP constr [] (replicate n WildP)
            conName = qualifiedConstrName constr
        body <- [| conName |]
        return $ Clause [pat] (NormalB body) []

-- | Get a qualified constructor name: "Module.Constructor" if module is available,
-- otherwise just "Constructor".
qualifiedConstrName :: Name -> String
qualifiedConstrName n = case nameModule n of
  Just m  -> m ++ "." ++ nameBase n
  Nothing -> nameBase n
