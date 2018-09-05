{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Derive.HFoldable
-- Copyright   :  (c) 2011 Patrick Bahr
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Automatically derive instances of @HFoldable@.
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Derive.HFoldable
    (
     HFoldable,
     makeHFoldable
    )where

import Control.Monad
import Data.Comp.Derive.Utils
import Data.Comp.Multi.HFoldable
import Data.Comp.Multi.HFunctor
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Language.Haskell.TH
import Prelude hiding (foldl, foldl1, foldr)
import qualified Prelude as P (foldl, foldl1, foldr)


iter 0 _ e = e
iter n f e = iter (n-1) f (f `appE` e)

iter' 0 _ e = e
iter' m f e = let f' = iter (m-1) [|fmap|] f
              in iter' (m-1) f (f' `appE` e)

iterSp n f g e = run n e
    where run 0 e = e
          run m e = let f' = iter (m-1) [|fmap|] (if n == m then g else f)
                    in run (m-1) (f' `appE` e)

{-| Derive an instance of 'HFoldable' for a type constructor of any higher-order
  kind taking at least two arguments. -}
makeHFoldable :: Name -> Q [Dec]
makeHFoldable fname = do
  Just (DataInfo _cxt name args constrs _deriving) <- abstractNewtypeQ $ reify fname
  let args' = init args
      fArg = VarT . tyVarBndrName $ last args'
      argNames = map (VarT . tyVarBndrName) (init args')
      complType = P.foldl AppT (ConT name) argNames
      classType = AppT (ConT ''HFoldable) complType
  constrs' <- mapM (mkPatAndVars . isFarg fArg <=< normalConExp) constrs
  foldDecl <- funD 'hfold (map foldClause constrs')
  foldMapDecl <- funD 'hfoldMap (map foldMapClause constrs')
  foldlDecl <- funD 'hfoldl (map foldlClause constrs')
  foldrDecl <- funD 'hfoldr (map foldrClause constrs')
  return [mkInstanceD [] classType [foldDecl,foldMapDecl,foldlDecl,foldrDecl]]
      where isFarg fArg (constr, args, gadtTy) = (constr, map (`containsType'` (getBinaryFArg fArg gadtTy)) args)
            filterVar [] _ = Nothing
            filterVar [d] x =Just (d, varE x)
            filterVar _ _ =  error "functor variable occurring twice in argument type"
            filterVars args varNs = catMaybes $ zipWith filterVar args varNs
            mkCPat constr args varNs = ConP constr $ zipWith mkPat args varNs
            mkPat [] _ = WildP
            mkPat _ x = VarP x
            mkPatAndVars (constr, args) =
                do varNs <- newNames (length args) "x"
                   return (mkCPat constr args varNs, filterVars args varNs)
            foldClause (pat,vars) =
                do let conApp (0,x) = [|unK $x|]
                       conApp (d,x) = iterSp d [|fold|] [| foldMap unK |] x
                   body <- if null vars
                           then [|mempty|]
                           else P.foldl1 (\ x y -> [|$x `mappend` $y|])
                                    $ map conApp vars
                   return $ Clause [pat] (NormalB body) []
            foldMapClause (pat,vars) =
                do fn <- newName "y"
                   let f = varE fn
                       f' 0 = f
                       f' n = iter (n-1) [|fmap|] [| foldMap $f |]
                       fp = if null vars then WildP else VarP fn
                   body <- case vars of
                             [] -> [|mempty|]
                             (_:_) -> P.foldl1 (\ x y -> [|$x `mappend` $y|]) $
                                      map (\ (d,z) -> iter' (max (d-1) 0) [|fold|] (f' d `appE` z)) vars
                   return $ Clause [fp, pat] (NormalB body) []
            foldlClause (pat,vars) =
                do fn <- newName "f"
                   en <- newName "e"
                   let f = varE fn
                       e = varE en
                       fp = if null vars then WildP else VarP fn
                       ep = VarP en
                       conApp x (0,y) = [|$f $x $y|]
                       conApp x (1,y) = [|foldl $f $x $y|]
                       conApp x (d,y) = let hidEndo = iter (d-1) [|fmap|] [|Endo . flip (foldl $f)|] `appE` y
                                            endo = iter' (d-1) [|fold|] hidEndo
                                        in [| appEndo $endo $x|]
                   body <- P.foldl conApp e vars
                   return $ Clause [fp, ep, pat] (NormalB body) []
            foldrClause (pat,vars) =
                do fn <- newName "f"
                   en <- newName "e"
                   let f = varE fn
                       e = varE en
                       fp = if null vars then WildP else VarP fn
                       ep = VarP en
                       conApp (0,x) y = [|$f $x $y|]
                       conApp (1,x) y = [|foldr $f $y $x |]
                       conApp (d,x) y = let hidEndo = iter (d-1) [|fmap|] [|Endo . flip (foldr $f)|] `appE` x
                                            endo = iter' (d-1) [|fold|] hidEndo
                                        in [| appEndo $endo $y|]
                   body <- P.foldr conApp e vars
                   return $ Clause [fp, ep, pat] (NormalB body) []
