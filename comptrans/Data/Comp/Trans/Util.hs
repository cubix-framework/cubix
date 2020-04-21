{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP             #-}

module Data.Comp.Trans.Util
  (
    AnnotationPropInfo(..)
  , annTyp
  , isAnnotation
  , propAnn
  , unpropAnn

  , TransCtx(..)
  , allTypes
  , substitutions
  , excludedNames
  , annotationProp

  , withAllTypes
  , withSubstitutions
  , withExcludedNames
  , withAnnotationProp
    
  , CompTrans
  , runCompTrans
    
  , standardExcludedNames
  , baseTypes
  , getLab
  , transName
  , nameLab
  , smartConstrName
  , modNameBase
  , simplifyDataInf
  , getTypeArgs
  , getNames
  , containsAll
  , getFullyAppliedType
  , getIsAnn
  , isPropagatingAnns
  , defaultPropAnn
  , defaultUnpropAnn
  , isVar
  , applySubsts
  , applyCurSubstitutions
  ) where

import Control.Lens ( (^.), (.~), _3, makeClassy, view )
import Control.Monad ( liftM2 )
import Control.Monad.Reader ( ReaderT(..), local )
import Control.Monad.Trans ( lift )

import Data.Data ( Data )
import Data.Generics ( everywhere, mkT )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( isJust )
import Data.Set ( Set, fromList )
import qualified Data.Set as Set

import Language.Haskell.TH.Syntax hiding ( lift )

import Data.ByteString ( ByteString )
import Data.Text ( Text )


type CompTrans = ReaderT TransCtx Q

data AnnotationPropInfo = AnnotationPropInfo { _annTyp       :: Type
                                             , _isAnnotation :: Type -> Bool
                                             , _propAnn      :: [(Exp, Type)] -> Exp
                                             , _unpropAnn    :: Exp -> Int -> [Exp]
                                             }

data TransCtx = TransCtx {
                           _allTypes      :: [Name]
                         , _substitutions :: Map Name Type
                         , _excludedNames :: Set Name
                         , _annotationProp :: Maybe AnnotationPropInfo
                         }

makeClassy ''AnnotationPropInfo
makeClassy ''TransCtx
  
defaultTransCtx :: TransCtx
defaultTransCtx = TransCtx {
                             _allTypes       = []
                           , _substitutions  = Map.empty
                           , _excludedNames  = standardExcludedNames
                           , _annotationProp = Nothing
                           }

runCompTrans :: CompTrans a -> Q a
runCompTrans m = runReaderT m defaultTransCtx


withAnnotationProp :: Type -> (Type -> Bool) -> ([(Exp, Type)] -> Exp) -> (Exp -> Int -> [Exp]) -> CompTrans a -> CompTrans a
withAnnotationProp annTyp isAnn propAnn unpropAnn = local (annotationProp .~ (Just $ AnnotationPropInfo annTyp isAnn propAnn unpropAnn))

withSubstitutions :: Map.Map Name Type -> CompTrans a -> CompTrans a
withSubstitutions substs = local (substitutions .~ substs)

withAllTypes :: [Name] -> CompTrans a -> CompTrans a
withAllTypes names = local (allTypes .~ names)

withExcludedNames :: Set Name -> CompTrans a -> CompTrans a
withExcludedNames names = local (excludedNames .~ names)

{-
   Names that should be excluded from an AST hierarchy.

   Type synonyms need not be present.
-}
standardExcludedNames :: Set Name
standardExcludedNames = fromList [''Maybe, ''Either, ''Int, ''Integer, ''Bool, ''Char, ''Double, ''Text, ''ByteString]


{-
   Types which should be translated into functorial form.
  
   Both String and its expansion are present because
   expandSyn threw errors
 -}
baseTypes :: [Type]
baseTypes = [ ConT ''Int
            , ConT ''Bool
            , ConT ''Char
            , ConT ''Float
            , ConT ''Double
            , ConT ''Integer
            , ConT ''String
            , AppT ListT (ConT ''Char)
            , ConT ''Text
            , ConT ''ByteString
            ]


getLab :: (Type -> Bool) -> Type -> CompTrans Type
getLab isAnn = gl
  where
    gl (AppT f@(AppT _ _) t) = liftM2 AppT (gl f) (gl t)
    gl (AppT c@(ConT n) t)
      | isAnn t = gl c
      | otherwise = do
          names <- view allTypes
          if elem n names then
            return $ ConT $ nameLab n
           else
            AppT (ConT n) <$> gl t
    gl (AppT f t) = AppT f <$> gl t
    gl ListT      = return ListT
    gl (TupleT n) = return $ TupleT n
    gl (ConT n)   = return $ ConT $ nameLab n
    gl x          = fail $ "When deriving multi-sorted compositional data type, found unsupported type in AST: " ++ show x


transName :: Name -> Name
transName = modNameBase id

nameLab :: Name -> Name
nameLab = modNameBase (++"L")

smartConstrName :: Name -> Name
smartConstrName = modNameBase ('i':)

modNameBase :: (String -> String) -> Name -> Name
modNameBase f = mkName . f . nameBase

simplifyDataInf :: Info -> [(Name, [Type])]
#if __GLASGOW_HASKELL__ < 800
simplifyDataInf (TyConI (DataD _ _ _ cons _))   = map extractCon cons
simplifyDataInf (TyConI (NewtypeD _ _ _ con _)) = [extractCon con]
#else
simplifyDataInf (TyConI (DataD _ _ _ _ cons _))   = map extractCon cons
simplifyDataInf (TyConI (NewtypeD _ _ _ _ con _)) = [extractCon con]
#endif
simplifyDataInf _                               = error "Attempted to derive multi-sorted compositional data type for non-nullary datatype"

extractCon :: Con -> (Name, [Type])
extractCon (NormalC nm sts) = (nm, map snd sts)
extractCon (RecC nm vsts)   = (nm, map (^. _3) vsts)
extractCon (ForallC _ _ c)  = extractCon c
extractCon _                = error "Unsupported constructor type encountered"

getTypeArgs :: Name -> CompTrans [Name]
getTypeArgs nm = do
  inf <- lift $ reify nm
  case inf of
#if __GLASGOW_HASKELL__ < 800
    TyConI (DataD _ _ tvs _ _)    -> return $ getNames tvs
    TyConI (NewtypeD _ _ tvs _ _) -> return $ getNames tvs
#else
    TyConI (DataD _ _ tvs _ _ _)    -> return $ getNames tvs
    TyConI (NewtypeD _ _ tvs _ _ _) -> return $ getNames tvs
#endif
    _                             -> return []

getNames :: [TyVarBndr] -> [Name]
getNames = map getName
  where
    getName :: TyVarBndr -> Name
    getName (PlainTV n)    = n
    getName (KindedTV n _) = n

containsAll :: (Ord a) => Map a b -> [a] -> Bool
containsAll mp = all (`Map.member` mp)

getFullyAppliedType :: Name -> CompTrans Type
getFullyAppliedType nm = do
  substs <- view substitutions
  typeArgs <- getTypeArgs nm
  return $ foldl AppT (ConT nm) (applySubsts substs $ map VarT typeArgs)

isPropagatingAnns :: CompTrans Bool
isPropagatingAnns = isJust <$> view annotationProp

getIsAnn :: CompTrans (Type -> Bool)
getIsAnn = do
  mApi <- view annotationProp
  case mApi of
    Nothing  -> return $ const False
    Just api -> return $ api ^. isAnnotation

-- | A default annotation propagater: Assumes 0 or 1 annotations per constructor
defaultPropAnn :: Exp -> [(Exp, Type)] -> Exp
defaultPropAnn defAnn tps = case tps of
      []       -> defAnn
      [(x, _)] -> x
      _        -> error "comptrans: Multiple annotation fields detected in constructor"

defaultUnpropAnn :: Exp -> Int -> [Exp]
defaultUnpropAnn _ 0 = []
defaultUnpropAnn x 1 = [x]
defaultUnpropAnn _ _ = error "comptrans: Multiple annotation fields detected in constructor"

isVar :: Type -> Bool
isVar (VarT n) = True
isVar _        = False

applySubsts :: (Data x) => Map Name Type -> x -> x
applySubsts mp = everywhere (mkT subst1)
  where
    subst1 :: Type -> Type
    subst1 t@(VarT n) = case Map.lookup n mp of
      Just res -> res
      Nothing  -> t
    subst1 t          = t

applyCurSubstitutions :: (Data x) => x -> CompTrans x
applyCurSubstitutions x = applySubsts <$> view substitutions <*> pure x
