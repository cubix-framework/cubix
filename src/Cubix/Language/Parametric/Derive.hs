{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
-- 
-- A utility wrapper around both Data.Comp.Multi.Derive and
-- the comptrans library, allowing us to easily derive all desired instances
-- for a higher-order functor, including for Cubix's typeclasses

module Cubix.Language.Parametric.Derive
  (
    deriveAll
  -- , distributeAnnotation
  -- , declareAnnotatedNames
  -- , declareAnnotated

  , deriveAllButDynCase

  , createSortInclusionType
  , createSortInclusionTypes
  , createSortInclusionInfer
  , createSortInclusionInfers

  , sumToNames
  , makeDefaultInstances
  ) where

import Control.Monad ( liftM )

import Language.Haskell.TH.ExpandSyns ( expandSyns )
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax hiding ( Cxt )

import Data.Comp.Multi ( (:&:), (:-<:), project', HFunctor, All )
import Data.Comp.Multi.Derive ( derive, makeHFunctor, makeHTraversable, makeHFoldable, makeEqHF, makeShowHF, makeOrdHF )
import Data.Comp.Multi.Strategy.Derive ( makeDynCase )

import Cubix.Language.Parametric.InjF
import Cubix.Sin.Compdata.Derive ( smartFConstructors )

--------------------------------------------------------------------------------

-- | Derives instances of the following for each type in the list:
-- 
-- @
-- 'HFunctor', 'HTraversable', 'HFoldable', 'EqHF', 'ShowHF', 'OrdHF', 'DynCase'
-- @
-- 
-- Additonally, it will create smart constructors for the data type
deriveAll :: [Name] -> Q [Dec]
deriveAll = derive [makeHFunctor, makeHTraversable, makeHFoldable, makeEqHF, makeShowHF,
                    makeOrdHF, smartFConstructors, makeDynCase]
  
deriveAllButDynCase :: [Name] -> Q [Dec]
deriveAllButDynCase = derive [makeHFunctor, makeHTraversable, makeHFoldable, makeEqHF, makeShowHF,
                    makeOrdHF, smartFConstructors]

-- TODO: distributeAnnotation
-- -- | Distributes an annotation over a sum
-- distributeAnnotation :: Type -> Name -> Type
-- distributeAnnotation typ ann = dist typ
--   where
--     dist :: Type -> Type
--     dist (AppT (AppT (ConT c) l) r)
--          | c == ''Sum             = AppT (AppT (ConT ''(:+:)) (dist l)) (dist r) -- parser complained about pattern
--     dist t                       = AppT (AppT (ConT ''(:&:)) t) (ConT ann)

-- declareAnnotatedType :: String -> Name -> Type -> [Dec]
-- declareAnnotatedType s ann typ = [TySynD (mkName s) [] annTyp]
--   where
--     annTyp = distributeAnnotation typ ann

-- -- | @declareAnnotatedNames s l ts@ will declare @s@ to be the sum of the @ts@, each annotated
-- -- with @l@. It corresponds roughly to the following type-level pseudocode:
-- -- 
-- -- @
-- -- type s = fold (:+:) (map (\t -> t :&: l) ts)
-- -- @
-- declareAnnotatedNames :: String -> Name -> [Name] -> Q [Dec]
-- declareAnnotatedNames _ _ []    = fail "declareAnnotatedNames: Name list empty"
-- declareAnnotatedNames s ann nms = return $ declareAnnotatedType s ann $ foldr typeSum (ConT $ last nms) (map ConT $ init nms)
--   where
--     typeSum a b = AppT (AppT (ConT ''(:+:)) a) b

-- declareAnnotated :: String -> Name -> Name -> Q [Dec]
-- declareAnnotated s ann nm = declareAnnotatedType s ann <$> expandSyns (ConT nm)


createSortInclusionType :: Name -> Name -> Q [Dec]
createSortInclusionType fromNm toNm = do
  let tName = sortInclusionName fromNm toNm
  e <- newName "e"
  i <- newName "i"
  let ctx = [foldl AppT EqualityT [VarT i, ConT toNm]]
  let notStrict = Bang NoSourceUnpackedness NoSourceStrictness
  let con = ForallC [] ctx $ NormalC tName [(notStrict, AppT (VarT e) (ConT fromNm))]
  return $ [DataD [] tName [KindedTV e (AppT (AppT ArrowT StarT) StarT), PlainTV i] Nothing [con] []]

  
createSortInclusionTypes :: [Name] -> [Name] -> Q [Dec]
createSortInclusionTypes froms tos = liftM concat $ mapM (uncurry createSortInclusionType) $ zip froms tos

-- | This is separated from createSortInclusionType because of phase limitations.
--  This needs to refer to the smart constructor, and deriving a smart constructor needs to reify
--  the type name.
createSortInclusionInfer :: Name -> Name -> Q [Dec]
createSortInclusionInfer fromNm toNm = do
    let tname = sortInclusionName fromNm toNm
    mkInjF tname
  where
    mkInjF :: Name -> Q [Dec]
    mkInjF tName = do
      let t     = conT tName
      let fromT = conT fromNm
      let toT   = conT toNm
      let smartCon = varE $ smartConName tName

      x <- newName "x"
      let p = conP tName [varP x]
      let xe = varE x

      [d| instance ($t :-<: fs, All HFunctor fs) => InjF fs $fromT $toT where
            injF = $smartCon

            projF' (project' -> Just $p) = Just $xe
            projF' _                     = Nothing
        |]


createSortInclusionInfers :: [Name] -> [Name] -> Q [Dec]
createSortInclusionInfers froms tos = liftM concat $ mapM (uncurry createSortInclusionInfer) $ zip froms tos

smartConName :: Name -> Name
smartConName n = mkName $ "i" ++ (nameBase n)


sortInclusionName :: Name -> Name -> Name
sortInclusionName fromNm toNm = mkName $ (chopL $ nameBase fromNm) ++ "Is" ++ (chopL $ nameBase toNm)

-- Be warned! This is coupled to the type label name scheme in comptrans
chopL :: String -> String
chopL s = if last s == 'L' then
            init s
          else
            s

-- | Takes something like "type Sig = '[ Add, Mul, Var]" and gives [''Add, ''Mul, ''Var]
sumToNames :: Name -> Q [Name]
sumToNames nm = do
    (TyConI (TySynD _ _ typ)) <- reify nm
    return $ extract typ
  where
    extract :: Type -> [Name]
    extract (SigT t _) = extract t
    extract (AppT (AppT PromotedConsT (ConT n)) res) = n : extract res
    extract PromotedNilT = []
    extract _ = error "sumToNames found invalid summand; only recognizes Sum and names"

-- | Used for creating default cases for Untrans instances.
--   There are two default untranslate cases (identity for shared functors, error for unshared),
--   so we need more advanced typeclass machinery to have both defaults. This caused GHC -O2
--   to go quadratic, and once caused a 64 GB server to run out of memory compiling.
--   So, we use TH instead for the error cases.
--
--  Update: Maybe the 64 GB thing was something else (it's still happening at link time),
--          but we still have pretty conclusive tests showing that our pattern of doing this with pure typeclass
--          machinery was making GHC go quadratic, and then switching to TH has substantially improved compilation times
makeDefaultInstances :: [Type] -> Name -> Name -> Exp -> [Dec]
makeDefaultInstances typs className methName exp =
  flip map typs $ \t -> let instSig = AppT (ConT className) t in
                        let dec = ValD (VarP methName) (NormalB exp) [] in
                        InstanceD Nothing [] instSig [dec]

