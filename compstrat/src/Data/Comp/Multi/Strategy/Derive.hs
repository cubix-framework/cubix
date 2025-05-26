{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP             #-}


-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Strategy.Derive
-- Copyright   :  James Koppel, 2013
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- This file gives a Template-Haskell generator for `DynCase`
-----------------------------------------------------------------------------


module Data.Comp.Multi.Strategy.Derive (
    makeDynCase
  ) where

import Control.Arrow ( (&&&) )
import Control.Monad

import Data.List ( nub )
import Data.Maybe ( catMaybes )
import Data.Type.Equality ( (:~:)(..) )

import Language.Haskell.TH hiding ( Cxt )

import Data.Comp.Multi.Strategy.Classification ( KDynCase, kdyncase )


-- | @makeDynCase ''T@ takes a datatype @T@ of kind @(* -> *) -> * -> *@ (i.e.: a signature in the
--   @compdata@ or @cubix-compdata@ library) and generates a `DynCase` instance for @T@.
makeDynCase :: Name -> Q [Dec]
makeDynCase fname = do
#if __GLASGOW_HASKELL__ < 800
          TyConI (DataD _cxt tname targs constrs _deriving) <- abstractNewtypeQ $ reify fname
#else
          TyConI (DataD _cxt tname targs _ constrs _deriving) <- abstractNewtypeQ $ reify fname
#endif
          let iVar = tyVarBndrName $ last targs
          let labs = nub $ catMaybes $ map (iTp iVar) constrs
          let cons = map (abstractConType &&& iTp iVar) constrs
          mapM (genDyn tname cons) labs
     where
       iTp :: Name -> Con -> Maybe Type
       iTp iVar (ForallC _ cxt t) =
                  -- Check if the GADT phantom type is constrained
                  case [y | AppT (AppT (ConT eqN) x) y <- cxt, x == VarT iVar, eqN == ''(~)] of
                    [] -> iTp iVar t
                    tp:_ -> Just tp
       iTp _iVar (GadtC _ _ (AppT _ tp)) =
                  case tp of
                    VarT _ -> Nothing
                    _      -> Just tp
       iTp _iVar (RecGadtC _ _ (AppT _ tp)) =
                  case tp of
                    VarT _ -> Nothing
                    _      -> Just tp
       iTp _ _ = Nothing
  
       genDyn :: Name -> [((Name, Int), Maybe Type)] -> Type -> Q Dec
       genDyn tname cons tp = do
           clauses <- liftM concat $ mapM (mkClause tp) cons
           let body = [FunD 'kdyncase clauses]
           instTp  <- forallT []
                              (return [])
                              (foldl appT (conT ''KDynCase) [conT tname, return tp])
           return $ InstanceD Nothing [] instTp body
  
       mkClause :: Type -> ((Name, Int), Maybe Type) -> Q [Clause]
       mkClause tp (con, Just tp')
                   | tp == tp' = return [Clause [conPat con] 
                                                (NormalB (AppE (ConE 'Just) (ConE 'Refl)))
                                                []]
       mkClause _ (con, _) = return [Clause [conPat con]
                                            (NormalB (ConE 'Nothing))
                                            []]
  
       conPat :: (Name, Int) -> Pat
       conPat (con, n) = ConP con [] (replicate n WildP)


{-|
  This is the @Q@-lifted version of 'abstractNewtypeQ.
-}
abstractNewtypeQ :: Q Info -> Q Info
abstractNewtypeQ = liftM abstractNewtype

{-|
  This function abstracts away @newtype@ declaration, it turns them into
  @data@ declarations.
-}
abstractNewtype :: Info -> Info
#if __GLASGOW_HASKELL__ < 800
abstractNewtype (TyConI (NewtypeD cxt name args constr derive))
    = TyConI (DataD cxt name args [constr] derive)
#else
abstractNewtype (TyConI (NewtypeD cxt name args mk constr derive))
    = TyConI (DataD cxt name args mk [constr] derive)
#endif
abstractNewtype owise = owise


{-|
  This function provides the name and the arity of the given data constructor.
-}
abstractConType :: Con -> (Name,Int)
abstractConType (NormalC constr args) = (constr, length args)
abstractConType (RecC constr args) = (constr, length args)
abstractConType (InfixC _ constr _) = (constr, 2)
abstractConType (ForallC _ _ constr) = abstractConType constr
abstractConType (GadtC [constr] args _) = (constr, length args)
abstractConType (RecGadtC [constr] args _) = (constr, length args)


{-|
  This function returns the name of a bound type variable
-}
tyVarBndrName (PlainTV n _) = n
tyVarBndrName (KindedTV n _ _) = n



{-|
  This function provides a list (of the given length) of new names based
  on the given string.
-}
newNames :: Int -> String -> Q [Name]
newNames n name = replicateM n (newName name)

