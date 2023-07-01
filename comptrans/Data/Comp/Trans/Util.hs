{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

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
    
  , CompTrans(..)
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
import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.Reader ( MonadReader, ReaderT(..), local )
import Control.Monad.Trans ( lift )

import Data.Data ( Data )
import Data.Generics ( everywhere, mkT )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( isJust )
import Data.Set ( Set, fromList )

import Language.Haskell.TH.Syntax hiding ( lift )

import Data.ByteString ( ByteString )
import Data.Text ( Text )

-- | Information used to propagate annotations from terms of third party language library to terms of @cubix-compdata@
--
--   Most libraries associate at most one annotation with each node of a term. However, there are occasional examples
--   where a library attaches multiple annotations to a single node (e.g.: a large construct may have multiple
--   position annotations attached to it). Most of the complexity of this type is constructed to deal with these cases,
--   giving the ability to combine multiple such annotations into a single value, and split a single value into
--   multiple annotations.
data AnnotationPropInfo =
        AnnotationPropInfo {
                             -- | The type of annotation to propagate
                             _annTyp       :: Type

                             -- | A test for whether a type is an annotation to propagate. Usually @(== annType)@.
                           , _isAnnotation :: Type -> Bool

                             -- | Annotation propagator: takes a list of TH expressions (and their types)
                             --   which at runtime evaluate to the annotations (as reported by isAnn) accompanying
                             --   a term. Returns a single TH expression constructing a new annotation to
                             --   be attached to the translated @cubix-compdata@ term.
                             --
                             --   `defaultPropAnn` suffices for most purposes, but users have the option to
                             --   define an annotation propagator which combines multiple annotations into one.
                           , _propAnn      :: [(Exp, Type)] -> Exp

                             -- | Annotation unpropagator; inverts @propAnn@. Takes a TH expression
                             --   which at runtime evaluates to the annotation accompanying a @cubix-compdata@ term,
                             --   along with the number of distinct annotation values to be included in the target
                             --   term. Returns a list of TH expressions which construct the annotations to be attached
                             --   to the untranslated term.
                             --
                             --   `defaultUnpropAnn` usually suffices, but users have the option
                             --   to define an annotation unpropagator which splits a single value into multiple.
                           , _unpropAnn    :: Exp -> Int -> [Exp]
                           }

-- | Configuration parameters for @comptrans@
data TransCtx = TransCtx {
                           -- | For internal use only
                           _allTypes      :: [Name]

                           -- | Used primarily for compatibility with libraries such as @language-c@,
                           --   where datatypes all take a parameter, e.g.: `CStmt a`, where @a@ is an annotation
                           --   parameter which essentially only has one value (e.g.: @SourceSpan@). This
                           --   substitutions map will be used to replace all such type variables with concrete types,
                           --   grounding all such datatypes to be kind @*@.
                           --
                           --   Other integrations that require this map for the same reason include @language-lua@
                           --   and @language-python@
                         , _substitutions :: Map Name Type

                           -- | A set of names to not generate definitions for, so that they may be handled
                           --   manually.
                         , _excludedNames :: Set Name                  -- ^

                           -- | When this is set, `deriveTrans` and `deriveUntrans` will generate code that
                           --   converts annotated terms in the integrated library into annotated @cubix-compdata@ terms,
                           --   with annotations given by @`(:&:)`@.
                           --
                           --   See documentation of `withAnnotationProp` and `AnnotationPropInfo`.
                         , _annotationProp :: Maybe AnnotationPropInfo -- ^
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


-- | The central monad of @comptrans@, defined as the `Q` monad with
--   additional configuration parameters
newtype CompTrans a = CompTrans { unCompTrans :: ReaderT TransCtx Q a }
  deriving ( Functor, Applicative, Monad, MonadFail, MonadIO, MonadReader TransCtx )

-- | Runs a @comptrans@ computation, resulting in a Template Haskell command
--   which creates some number of declarations.
--
--   `CompTrans` values are created by `deriveMulti`, `deriveTrans`, and `deriveUntrans`,
--   and may be configured using functions such as `withSubstitutions`
runCompTrans :: CompTrans a -> Q a
runCompTrans m = runReaderT (unCompTrans m) defaultTransCtx


withAnnotationProp :: Type                   -- ^ @annTyp@, the annotation type being propagated
                   -> (Type -> Bool)         -- ^ A test for whether a type is an annotation to propagate. Usually @(== annType)@.
                   -> ([(Exp, Type)] -> Exp) -- ^ Annotation propogater. Usually constructed with `defaultPropAnn`. See `AnnotationPropInfo`.
                   -> (Exp -> Int -> [Exp])  -- ^ Annotation unpropater. Usuallyl `defaultUnpropAnn`.  See `AnnotationPropInfo`
                   -> CompTrans a            -- ^ code generating @comptrans@ declarations
                   -> CompTrans a
withAnnotationProp annTyp isAnn propAnn unpropAnn = local (annotationProp .~ (Just $ AnnotationPropInfo annTyp isAnn propAnn unpropAnn))

-- | Runs a @comptrans@ declaration with a given set of type variable substitutions
withSubstitutions :: Map.Map Name Type -> CompTrans a -> CompTrans a
withSubstitutions substs = local (substitutions .~ substs)

withAllTypes :: [Name] -> CompTrans a -> CompTrans a
withAllTypes names = local (allTypes .~ names)

-- | Runs a @comptrans@ declaration with a given set of excluded namess
withExcludedNames :: Set Name -> CompTrans a -> CompTrans a
withExcludedNames names = local (excludedNames .~ names)

-- | Names that should be excluded from an AST hierarchy.
--   Includes base types, basic containers (`Maybe`, `Either`), and `Text`/`ByteString`.
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
simplifyDataInf (TyConI (DataD _ _ _ _ cons _))   = map extractCon cons
simplifyDataInf (TyConI (NewtypeD _ _ _ _ con _)) = [extractCon con]
simplifyDataInf _                               = error "Attempted to derive multi-sorted compositional data type for non-nullary datatype"

extractCon :: Con -> (Name, [Type])
extractCon (NormalC nm sts) = (nm, map snd sts)
extractCon (RecC nm vsts)   = (nm, map (^. _3) vsts)
extractCon (ForallC _ _ c)  = extractCon c
extractCon _                = error "Unsupported constructor type encountered"

getTypeArgs :: Name -> CompTrans [Name]
getTypeArgs nm = do
  inf <- CompTrans $ lift $ reify nm
  case inf of
    TyConI (DataD _ _ tvs _ _ _)    -> return $ getNames tvs
    TyConI (NewtypeD _ _ tvs _ _ _) -> return $ getNames tvs
    _                             -> return []

getNames :: [TyVarBndr a] -> [Name]
getNames = map getName
  where
    getName :: TyVarBndr a -> Name
    getName (PlainTV n _)    = n
    getName (KindedTV n _ _) = n

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

-- | A default annotation propagator: Assumes 0 or 1 annotations per constructor
--   Returns the default annotation or copies the given annotation as appropriate.
defaultPropAnn :: Exp -- ^ default annotation, to be used on terms that do not have an annotation
               -> [(Exp, Type)] -> Exp
defaultPropAnn defAnn tps = case tps of
      []       -> defAnn
      [(x, _)] -> x
      _        -> error "comptrans: Multiple annotation fields detected in constructor"

-- | A default annotation unpropagator: Assumes 0 or 1 annotations per constructor. If 1 annotation given, copies it.
defaultUnpropAnn :: Exp -> Int -> [Exp]
defaultUnpropAnn _ 0 = []
defaultUnpropAnn x 1 = [x]
defaultUnpropAnn _ _ = error "comptrans: Multiple annotation fields detected in constructor"

isVar :: Type -> Bool
isVar (VarT _) = True
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
