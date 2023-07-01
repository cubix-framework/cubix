{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Derive.HFunctor
-- Copyright   :  (c) 2011 Patrick Bahr
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Automatically derive instances of @HFunctor@.
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Derive.HFunctor
    (
     HFunctor,
     makeHFunctor
    ) where

import Control.Monad
import Data.Comp.Derive.Utils
import Data.Comp.Multi.HFunctor
import Data.Maybe
import Language.Haskell.TH
import Prelude hiding (mapM)
import qualified Prelude as P (mapM)

iter 0 _ e = e
iter n f e = iter (n-1) f (f `appE` e)

{-| Derive an instance of 'HFunctor' for a type constructor of any higher-order
  kind taking at least two arguments. -}
makeHFunctor :: Name -> Q [Dec]
makeHFunctor fname = do
  Just (DataInfo _cxt name args constrs _deriving) <- abstractNewtypeQ $ reify fname
  let args' = init args
      fArg = VarT . tyVarBndrName $ last args'
      argNames = map (VarT . tyVarBndrName) (init args')
      complType = foldl AppT (ConT name) argNames
      classType = AppT (ConT ''HFunctor) complType
  constrs' <- P.mapM (mkPatAndVars . isFarg fArg <=< normalConExp) constrs
  hfmapDecl <- funD 'hfmap (map hfmapClause constrs')
  return [mkInstanceD [] classType [hfmapDecl]]
      where isFarg fArg (constr, args, ty) = (constr, map (`containsType'` getBinaryFArg fArg ty) args)
            filterVar _ nonFarg [] x  = nonFarg x
            filterVar farg _ [depth] x = farg depth x
            filterVar _ _ _ _ = error "functor variable occurring twice in argument type"
            filterVars args varNs farg nonFarg = zipWith (filterVar farg nonFarg) args varNs
            mkCPat constr varNs = ConP constr [] $ map mkPat varNs
            mkPat = VarP

            mkPatAndVars :: (Name, [[t]]) -> Q (Q Exp, Pat, (t -> Q Exp -> c) -> (Q Exp -> c) -> [c], Bool, [Q Exp], [(t, Name)])
            mkPatAndVars (constr, args) =
                do varNs <- newNames (length args) "x"
                   return (conE constr, mkCPat constr varNs,
                           \ f g -> filterVars args varNs (\ d x -> f d (varE x)) (g . varE),
                           any (not . null) args, map varE varNs, catMaybes $ filterVars args varNs (curry Just) (const Nothing))
            hfmapClause (con, pat,vars',hasFargs,_,_) =
                do fn <- newName "f"
                   let f = varE fn
                       fp = if hasFargs then VarP fn else WildP
                       vars = vars' (\d x -> iter d [|fmap|] f `appE` x) id
                   body <- foldl appE con vars
                   return $ Clause [fp, pat] (NormalB body) []
