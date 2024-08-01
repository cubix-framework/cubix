{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Derive.SmartConstructors
-- Copyright   :  Original (c) 2011 Patrick Bahr; current version (c) 2024 James Koppel
-- License     :  BSD3
-- Maintainer  :  James Koppel <jkoppel@mit.edu>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Automatically derive smart constructors for mutually recursive types.
--
--------------------------------------------------------------------------------

module Data.Comp.Multi.Derive.SmartConstructors
    (
      smartConstructors
    , patternSynonyms
    ) where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Comp.Derive.Utils
import Data.Comp.Multi.Sum
import Data.Comp.Multi.Term
import Language.Haskell.TH hiding (Cxt)

-----------------------------------------------------------------------------------

getSortsFromConstructorType :: Name -> Con -> Maybe ([Type], Type)
getSortsFromConstructorType iVar (ForallC _ cxt t) =
-- Check if the GADT phantom type is constrained
  case [y | AppT (AppT (ConT eqN) x) y <- cxt, x == VarT iVar, eqN == ''(~)] of
    [] -> getSortsFromConstructorType iVar t
    tp:_ -> let args = case t of
                        NormalC _ vs -> map snd vs
                        RecC _ vs -> map (\(_, _, v) -> v) vs
                        _ -> []
            in Just (args, tp)
getSortsFromConstructorType _iVar (GadtC _ vs (AppT _ tp)) =
  case tp of
    VarT _ -> Nothing
    _      -> Just (map snd vs, tp)
getSortsFromConstructorType _iVar (RecGadtC _ vs (AppT _ tp)) =
  case tp of
    VarT _ -> Nothing
    _      -> Just (map (\(_, _, v) -> v) vs, tp)
getSortsFromConstructorType _ _ = Nothing

{-| Derive smart constructors for a type constructor of any higher-order kind
 taking at least two arguments. The smart constructors are similar to the
 ordinary constructors, but an 'inject' is automatically inserted. -}
smartConstructors :: Name -> Q [Dec]
smartConstructors fname = do
    Just (DataInfo _cxt tname targs constrs _deriving) <- abstractNewtypeQ $ reify fname
    let iVar = tyVarBndrName $ last targs
    let cons = map (abstractConType &&& (fmap snd . getSortsFromConstructorType iVar)) constrs
    liftM concat $ mapM (genSmartConstr (map tyVarBndrName targs) tname) cons
        where 
              genSmartConstr targs tname ((name, args), mgetSortsFromConstructorType) = do
                let bname = nameBase name
                genSmartConstr' targs tname (mkName $ 'j' : bname) name args mgetSortsFromConstructorType
              genSmartConstr' targs tname sname name args mgetSortsFromConstructorType = do
                varNs <- newNames args "x"
                let pats = map varP varNs
                    vars = map varE varNs
                    val = foldl appE (conE name) vars
                    sig = genSig targs tname sname args mgetSortsFromConstructorType
                    function = [funD sname [clause pats (normalB [|inject $val|]) []]]
                sequence $ sig ++ function
              genSig targs tname sname 0 mgetSortsFromConstructorType = (:[]) $ do
                fvar <- newName "f"
                hvar <- newName "h"
                avar <- newName "a"
                ivar <- newName "i"
                let targs' = init $ init targs
                    vars = hvar:fvar:avar:maybe [ivar] (const []) mgetSortsFromConstructorType++targs'
                    f = varT fvar
                    h = varT hvar
                    a = varT avar
                    i = varT ivar
                    ftype = foldl appT (conT tname) (map varT targs')
                    constr = classP ''(:<:) [ftype, f]
                    typ = foldl appT (conT ''Cxt) [h, f, a, maybe i return mgetSortsFromConstructorType]

                    -- NOTE 2023.06.29: Unsure if SpecifiedSpec is actually what we want to get
                    --                  reasonable type application on smart constructors
                    typeSig = forallT (map (\v -> PlainTV v SpecifiedSpec) vars) (sequence [constr]) typ
                sigD sname typeSig
              genSig _ _ _ _ _ = []

{-| 
  Example:
  data ExpL
  ...
 
  data Arith e l where
      Add :: e ExpL -> e ExpL -> Arith e ExpL

  patternSynonyms ''Arith

  ===>

  pattern Add' :: (Arith :<: f) => Cxt h f a ExpL -> Cxt h f a ExpL -> Cxt h f a ExpL
  pattern Add' a b <- (project -> Just (Add a b)) where
    Add' a b = inject $ Add a b

-}

patternSynonyms :: Name -> Q [Dec]
patternSynonyms fname = do
    Just (DataInfo _cxt tname targs constrs _deriving) <- abstractNewtypeQ $ reify fname
    let iVar = tyVarBndrName $ last targs
    let cons = map (abstractConType &&& getSortsFromConstructorType iVar) constrs
    liftM concat $ mapM (genPatternSyn (map tyVarBndrName targs) tname) cons
        where           
              genPatternSyn :: [Name] -> Name -> ((Name, Int), Maybe ([Type], Type)) -> Q [Dec]
              genPatternSyn targs tname ((constructorName, nArgs), maybeSorts) = do
                let constructorBaseName = nameBase constructorName
                let patternSynName = mkName (constructorBaseName <> "'")
                patSynArgNames <- newNames nArgs "x"
                let patSynArgs = PrefixPatSyn patSynArgNames 

                let pats = map varP patSynArgNames
                    vars = map varE patSynArgNames
                    val = foldl appE (conE constructorName) vars
                forwardBody <- clause pats (normalB [|inject $val|]) []

                backwardBody <- viewP [|project|] $ pure (ConP 'Just [] [ConP constructorName [] (map VarP patSynArgNames)])
                let patternSyn = PatSynD patternSynName patSynArgs (ExplBidir [forwardBody]) backwardBody

                sig <- genSig targs tname patternSynName maybeSorts
                return [sig, patternSyn]

              genSig :: [Name] -> Name -> Name -> Maybe ([Type], Type) -> Q Dec
              genSig targs tname sname maybeSorts = do
                fvar <- newName "f"
                hvar <- newName "h"
                avar <- newName "a"
                jvar <- newName "j"
                let targs' = init $ init targs
                    vars = hvar:fvar:avar:[jvar]++targs'
                    f = varT fvar
                    h = varT hvar
                    a = varT avar
                    j = varT jvar
                    ftype = foldl appT (conT tname) (map varT targs')
                    typGen = foldl appT (conT ''Cxt) [h, f, a]
                    (args, returnType) = case maybeSorts of
                      Nothing -> ([], appT typGen j)
                      Just (as, ret) -> (map (mkArgType h f a) as, mkRetType h f a ret)
                    typ = arrow (args ++ [returnType])
                    constr = classP ''(:<:) [ftype, f]
                    --constr' = classP ''All [conT ''HFunctor, varT fvar]

                    -- NOTE 2023.06.29: Unsure if SpecifiedSpec is what we want here to get working type applications
                    typeSig = forallT (map (\v -> PlainTV v SpecifiedSpec) vars) (sequence [constr]) typ
                patSynSigD sname typeSig

              mkArgType :: Q Type -> Q Type -> Q Type -> Type -> Q Type
              mkArgType h f a (AppT (VarT _) t) =
                -- NOTE: e ( .. ) case
                foldl appT (conT ''Cxt) [h, f, a, pure t]
              mkArgType _ _ _ t =
                pure t

              mkRetType :: Q Type -> Q Type -> Q Type -> Type -> Q Type
              mkRetType h f a t =
                foldl appT (conT ''Cxt) [h, f, a, pure t]

              arrow =
                foldr1 (\a acc -> arrowT `appT` a `appT` acc)
                
