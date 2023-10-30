{-# LANGUAGE TemplateHaskell #-}

{-|
   This is part of an experiment in using sort-inclusions (i.e. injf) in parametric syntax.
   This file is sinful because it is a code-clone of the smartConstructors generator in the compdata library,
   and also copies Data.Comp.Derive.Utils verbatim
  
   After being validated and reaching maturity, this should be pull-requested in to the real compdata
   library (or the alternative, perhaps better solution, of redefining inject to what is currently called injectF)
 -}

module Cubix.Sin.Compdata.Derive (
    smartFConstructors
  ) where

import Control.Arrow ( (&&&) )
import Control.Monad

import Data.Comp.Multi.Sum
import Data.Comp.Multi.Term

import Language.Haskell.TH hiding ( Cxt )

import Cubix.Language.Parametric.InjF ( InjF, injectF )

smartFConstructors :: Name -> Q [Dec]
smartFConstructors fname = do
    TyConI (DataD _cxt tname targs _ constrs _deriving) <- abstractNewtypeQ $ reify fname
    let iVar = tyVarBndrName $ last targs
    let cons = map (abstractConType &&& iTp iVar) constrs
    liftM concat $ mapM (genSmartConstr (map tyVarBndrName targs) tname) cons
        where iTp iVar (ForallC _ cxt t) =
                  -- Check if the GADT phantom type is constrained
                  case [y | AppT (AppT (ConT eqN) x) y <- cxt, x == VarT iVar, eqN == ''(~)] of
                    [] -> iTp iVar t
                    tp:_ ->
                      let args = case t of
                            NormalC _ vs -> map snd vs
                            RecC _ vs -> map (\(_, _, v) -> v) vs
                            _ -> []
                      in Just (args, tp)
              iTp _iVar (GadtC _ vs (AppT _ tp)) =
                  case tp of
                    VarT _ -> Nothing
                    _      -> Just (map snd vs, tp)
              iTp _iVar (RecGadtC _ vs (AppT _ tp)) =
                  case tp of
                    VarT _ -> Nothing
                    _      -> Just (map (\(_, _, v) -> v) vs, tp)
              iTp _ _ = Nothing
              genSmartConstr targs tname ((name, args), miTp) = do
                let bname = nameBase name
                genSmartConstr' targs tname (mkName $ 'i' : bname) name args miTp
              genSmartConstr' targs tname sname name args miTp = do
                varNs <- newNames args "x"
                let pats = map varP varNs
                    vars = map varE varNs
                    val = foldl appE (conE name) vars
                    sig = genSig targs tname sname args miTp
                    body = maybe [|inject $val|] (const [|injectF $val|]) miTp
                    function = [funD sname [clause pats (normalB body) []]]
                sequence $ sig ++ function
              genSig targs tname sname tys miTp = (:[]) $ do
                fsvar <- newName "fs"
                hvar <- newName "h"
                avar <- newName "a"
                jvar <- newName "j"
                let targs' = init $ init targs
                    vars = hvar:fsvar:avar:[jvar]++targs'
                    fs = varT fsvar
                    h = varT hvar
                    a = varT avar
                    j = varT jvar
                    ftype = foldl appT (conT tname) (map varT targs')
                    typGen = foldl appT (conT ''CxtS) [h, fs, a]
                    args = case fst <$> miTp of
                      Nothing -> []
                      Just as -> map (mkArgType h fs a) as
                    typ = arrow (args ++ [appT typGen j])
                    constr = classP ''(:-<:) [ftype, fs]
                    constr' = classP ''InjF [fs, maybe j (pure . snd) miTp, j]

                    -- NOTE 2023.06.29: Unsure if SpecifiedSpec is what we want here to get working type applications
                    typeSig = forallT (map (\v -> PlainTV v SpecifiedSpec) vars) (sequence [constr, constr']) typ
                sigD sname typeSig

              mkArgType h fs a (AppT (VarT _) t) =
                -- NOTE: e ( .. ) case
                foldl appT (conT ''CxtS) [h, fs, a, pure t]
              mkArgType _ _ _ t =
                pure t
              arrow =
                foldr1 (\a acc -> arrowT `appT` a `appT` acc)



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
abstractNewtype (TyConI (NewtypeD cxt name args mk constr derive))
    = TyConI (DataD cxt name args mk [constr] derive)
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

