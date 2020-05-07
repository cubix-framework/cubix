{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Cubix.Transformations.Hoist.Custom (
    HoistState
  , seenIdents
  , HoistStateConstraints(..)
  , VarInitToRhs(..)
  , VarDeclBinderToLhs(..)
  , SpecialHoist(..)
  , BuiltinSpecialIdents(..)
  , BlockHoisting(..)
  ) where


import Control.Monad.Identity ( runIdentity )
import Control.Monad.State ( MonadState )

import Data.Proxy ( Proxy(..) )
import Data.Set ( Set )
import qualified Data.Set as Set

import Control.Lens ( makeLenses, (^.), (%=) )

import Data.Comp.Multi ( project, HFix, Sum )
import Data.Comp.Multi.Strategic ( Rewrite, GRewrite, allbuR, alltdR, promoteR, addFail )
import Data.Comp.Multi.Strategy.Classification ( subterms )

import Cubix.Language.C.Parametric.Common as CCommon
import qualified Cubix.Language.C.Parametric.Full as CFull
import Cubix.Language.Java.Parametric.Common as JCommon
import Cubix.Language.JavaScript.Parametric.Common as JSCommon
import Cubix.Language.Lua.Parametric.Common as LCommon

import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax.Base
import Cubix.Language.Parametric.Syntax.Functor as P

import Cubix.Transformations.Variation

type family SpecialHoistState (f :: (* -> *) -> * -> *) :: *

data HoistState f = HoistState {
                                 _seenIdents   :: Set String
                               , _specialState :: SpecialHoistState f
                               }

makeLenses ''HoistState

type instance SpecialHoistState (Sum MLuaSig) = ()

#ifndef ONLY_ONE_LANGUAGE
type instance SpecialHoistState MCSig = ()
type instance SpecialHoistState MJSSig = ()

data JavaSpecialHoistState = JavaSpecialHoistState {
                               _localClasses :: Set String
                             }

makeLenses ''JavaSpecialHoistState

type instance SpecialHoistState MJavaSig = JavaSpecialHoistState
#endif

class HoistStateConstraints f where
  emptyHoistState :: HoistState f
  updateSpecialState :: (MonadState (HoistState f) m) => HFix f l -> m ()

instance {-# OVERLAPPABLE #-} (SpecialHoistState f ~ ()) => HoistStateConstraints f where
  emptyHoistState = HoistState Set.empty ()
  updateSpecialState _ = return ()

#ifndef ONLY_ONE_LANGUAGE
instance {-# OVERLAPPING #-} HoistStateConstraints MJavaSig where
  emptyHoistState = HoistState Set.empty (JavaSpecialHoistState Set.empty)

  updateSpecialState x
     | Just (LocalClass d) <- project x
     , Just (ClassDecl _ n _ _ _ _) <- project d
     , Just (Ident' s) <- projF n
     = specialState.localClasses %= Set.insert s


  updateSpecialState x
     | Just (LocalClass d) <- project x
     , Just (EnumDecl _ n _ _) <- project d
     , Just (Ident' s) <- projF n
     = specialState.localClasses %= Set.insert s
  updateSpecialState _ = return ()
#endif


class VarInitToRhs f where
  varInitToRhs :: f MultiLocalVarDeclCommonAttrsL -> f VarDeclBinderL -> f LocalVarDeclAttrsL -> f LocalVarInitL -> f RhsL

#ifndef ONLY_ONE_LANGUAGE
instance VarInitToRhs MCTerm where
  varInitToRhs cAttrs (projF -> Just (Ident' nam)) lAttrs (projF -> Just (x :: MCTerm CInitializerL)) = case project x of
    Just (CInitExpr e _) -> injF e
    Just (CInitList l ann) -> iCCompoundLit decl l ann
      where
        decl = iCDecl (SingletonF' (iCTypeSpec (iCTypeOfExpr (iCVar (iIdent nam) iUnitF) iUnitF))) riNilF iUnitF

instance VarInitToRhs MJavaTerm where
  varInitToRhs cAttrs _ lAttrs (projF -> Just (x :: MJavaTerm VarInitL)) = case project x of
    Just (InitExp e)   -> injF e
    Just (InitArray a) -> iArrayCreateInit t dim a
      where
        Just (ArrayDimVarDeclAttrs dim) = project lAttrs
        Just (cAttrPair :: MJavaTerm ([ModifierL], TypeL)) = projF cAttrs
        t = snd $ extractF2 cAttrPair

instance VarInitToRhs MJSTerm where
  varInitToRhs _ _ _ (projF -> Just (x :: MJSTerm JSExpressionL)) = injF x
#endif

instance VarInitToRhs MLuaTerm where
  varInitToRhs _ _ _ (projF -> Just (x :: MLuaTerm [LCommon.ExpL])) = injF x

class VarDeclBinderToLhs f where
  varDeclBinderToLhs :: f VarDeclBinderL -> f LhsL

#ifndef ONLY_ONE_LANGUAGE
instance VarDeclBinderToLhs MCTerm where
  varDeclBinderToLhs (fromProjF -> n :: MCTerm IdentL) = injF n

instance VarDeclBinderToLhs MJavaTerm where
  varDeclBinderToLhs (fromProjF -> n :: MJavaTerm IdentL) = injF n

instance VarDeclBinderToLhs MJSTerm where
  varDeclBinderToLhs (fromProjF -> e :: MJSTerm JSExpressionL) = injF e
#endif

instance VarDeclBinderToLhs MLuaTerm where
  varDeclBinderToLhs (fromProjF -> TupleBinder' ns) = injF $ mapF identToVar ns
    where
      identToVar :: MLuaTerm IdentL -> MLuaTerm VarL
      identToVar = iVarName . injF


-- Created this instead of messing with the normal hoist pipeline so that elementary
-- hoist can remain unchanged
class SpecialHoist f where
  -- Let's not even pretend to support this
  --specialHoistSingle :: HoistState f -> Term f SingleLocalVarDeclL -> Maybe (Term f [BlockItemL], Term f [StatSort f])

  specialHoistMulti  :: HoistState f
                     -> HFix f MultiLocalVarDeclCommonAttrsL
                     -> HFix f LocalVarDeclAttrsL
                     -> HFix f VarDeclBinderL
                     -> HFix f OptLocalVarInitL
                     -> Maybe ([HFix f BlockItemL], [HFix f (StatSort f)])

instance {-# OVERLAPPABLE #-} SpecialHoist f where
  --specialHoistSingle _ _ = Nothing
  specialHoistMulti _ _ _ _ _ = Nothing

#ifndef ONLY_ONE_LANGUAGE

-- Cheap test; erroneously fires on some function pointers. But,
-- in the current implementation, it will only lead to incorrect results
-- if the function pointer is assigned to a string...
isArray :: MCTerm LocalVarDeclAttrsL -> Bool
isArray t = not $ null (subterms t :: [MCTerm CArraySizeL])

-- Also cheap....and we only do int constants.
-- The purpose of this is that character arrays initialized by a string literal must have
-- uninitialized elements zeroed. We hence need to generate code to zero them.
-- See 20000801-4.c from the GCC torture tests.
getCharArraySize :: MCTerm LocalVarDeclAttrsL -> Maybe Int
getCharArraySize t = case head (subterms t :: [MCTerm CArraySizeL]) of
  (project -> Just (CArrSize _ (project -> Just (CConst (project -> Just (CIntConst (CInteger' n) _)))))) -> Just (fromInteger n)
  _ -> Nothing



updateSize :: MCTerm l -> Int -> MCTerm l
updateSize t n = runIdentity $ (alltdR $ promoteR $ addFail $ updateSize' n) t

updateSize' :: Int -> Rewrite MCTerm CArraySizeL
updateSize' n (project -> Just (CNoArrSize _)) = return $ iCArrSize False (iCConst $ iCIntConst (CInteger' (toInteger n)) iUnitF)
updateSize' _ t                                = return t

makeDeclUpdateSize attrs lattrs nam len = updateSize decl len
  where
    decl = iMultiLocalVarDecl attrs (SingletonF' (SingleLocalVarDecl' lattrs (iIdent nam) NoLocalVarInit'))


makeMemcpy nam val = memcpy
  where
    var = iCVar (iIdent nam) iUnitF
    argList = map PositionalArgument' [ iCUnary iCAdrOp var iUnitF
                                      , iCUnary iCAdrOp val iUnitF
                                      , iCSizeofExpr var iUnitF
                                      ]

    memcpy = iCBlockStmt (iCExpr (Just' $ iFunctionCall EmptyFunctionCallAttrs'
                                                        (iCVar (iIdent "memcpy") iUnitF)
                                                        (FunctionArgumentList' $ insertF argList))
                                 iUnitF)

hoistArrayInitString :: MCTerm MultiLocalVarDeclCommonAttrsL
                     -> MCTerm LocalVarDeclAttrsL
                     -> String
                     -> MCTerm CExpressionL
                     -> Maybe ([MCTerm BlockItemL], [MCTerm BlockItemL])
hoistArrayInitString attrs lattrs nam (project -> Just (CConst (project -> Just (CStrConst (CString' str) _)))) =
    Just ([decl], [memcpy])
  where
    decl = makeDeclUpdateSize attrs lattrs nam (length str + 1)

    arrSize = maybe 0 id (getCharArraySize lattrs)
    paddedStr = str ++ (take (arrSize - length str) $ repeat '\0')
    memcpy = makeMemcpy nam (iCConst $ iCStrConst (CString' paddedStr) iUnitF)


hoistArrayInitString _ _ _ _ = Nothing



instance {-# OVERLAPPABLE #-} SpecialHoist MCSig where
  --specialHoistSingle _ _ = Nothing


  -- We turn "int x[3] = {1,2,3};" into "int x[3];" and "memcpy(&x, (typeof(x)){1,2,3}, sizeof(x));"
  specialHoistMulti _ attrs lattrs (projF -> Just (Ident' nam)) init
    | JustLocalVarInit' linit <- init
    , Just (x :: MCTerm CInitializerL) <- projF linit = case project x of
             Just (CInitExpr e _) -> if isArray lattrs then
                                       hoistArrayInitString attrs lattrs nam e
                                     else
                                       Nothing
             Just (CInitList l _) -> Just ([decl], [memcpy])
               where
                 litType = iCDecl (SingletonF' (iCTypeSpec (iCTypeOfExpr (iCVar (iIdent nam) iUnitF) iUnitF))) riNilF iUnitF
                 compoundLit = iCCompoundLit litType l iUnitF :: MCTerm CExpressionL
                 memcpy = makeMemcpy nam compoundLit

                 decl = makeDeclUpdateSize attrs lattrs nam (length $ extractF l)



  specialHoistMulti _ _ _ _ _ = Nothing
#endif

-- Dynamic languages
--
-- For instance, in Lua, changing "local _ENV = x" to "local _ENV; _ENV = x" will have the effect
-- of temporarily setting _ENV to nil, which then renders all variables (including "x") inacessible
class BuiltinSpecialIdents (f :: (* -> *) -> * -> *) where
  builtinReferencedIdents :: Proxy f -> Set String

instance {-# OVERLAPPABLE #-} BuiltinSpecialIdents f where
  builtinReferencedIdents _ = Set.empty

instance {-# OVERLAPPING #-} BuiltinSpecialIdents (Sum MLuaSig) where
  builtinReferencedIdents _ = Set.fromList ["_ENV"]


-- The identifier set should probably be generalized to a language-specific hoist state
class BlockHoisting f where
  blockHoistSingle :: HoistState f -> HFix f SingleLocalVarDeclL -> Bool
  blockHoistMulti  :: HoistState f -> HFix f MultiLocalVarDeclL  -> Bool

instance {-# OVERLAPPABLE #-} BlockHoisting f where
  blockHoistSingle _ _ = False
  blockHoistMulti  _ _ = False

#ifndef ONLY_ONE_LANGUAGE


referencedIdents :: MCTerm l -> Set String
referencedIdents t = Set.fromList $ map (\(Ident' s) -> s) $ subterms t

-- A bad check for if it has a variable array length
hasReffedIdents :: Set String -> MCTerm l -> Bool
hasReffedIdents seenIds t = not $ Set.null $ Set.intersection seenIds (referencedIdents t)

-- This is a bad check that will erroneously report that "const int*" is const
-- (It's not, but "int * const" is.)
containsConstAttr :: MCTerm l -> Bool
containsConstAttr a = any isConstQual (subterms a :: [MCTerm CTypeQualifierL])
  where
    isConstQual :: MCTerm CTypeQualifierL -> Bool
    isConstQual (project -> Just (CConstQual _)) = True
    isConstQual _                                = False

instance {-# OVERLAPPING #-} BlockHoisting MCSig where
  blockHoistSingle state (SingleLocalVarDecl' a n _) =
      hasReffedIdents (state ^. seenIdents) a || containsConstAttr a

  blockHoistMulti  state (MultiLocalVarDecl'  a ds)  =
      hasReffedIdents (state ^. seenIdents) a || containsConstAttr a || any (blockHoistSingle state) (extractF ds)

isLocalClassType :: Set String -> MJavaTerm l -> Bool
isLocalClassType locClasses t = not $ Set.null $ Set.intersection locClasses classRefTypes
  where
    classRefTypes :: Set String
    classRefTypes = Set.fromList $ do
      refTypes <- subterms t :: [MJavaTerm ClassTypeL]
      Ident' n <- subterms refTypes :: [MJavaTerm IdentL]
      return n

instance {-# OVERLAPPING #-} BlockHoisting MJavaSig where
  blockHoistSingle _ _ = False
  blockHoistMulti state (MultiLocalVarDecl' attrs _) = isLocalClassType (state ^. specialState ^. localClasses) attrs
#endif
