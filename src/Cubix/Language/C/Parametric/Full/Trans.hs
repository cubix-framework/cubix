{-# OPTIONS_HADDOCK hide #-}
--{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE CPP                     #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE UndecidableInstances    #-}

-- This is a separate file due to GHC's phase restriction.

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.C.Parametric.Full.Trans () where
#else
module Cubix.Language.C.Parametric.Full.Trans (
    translate
  , translateExtDecl
  , translateStatement
  , translateExpression
  , translateNodeInfo
  , untranslate
  ) where

import Data.Typeable ( Typeable )

import qualified Language.C as C
import qualified Language.Haskell.TH as TH

import Data.Comp.Multi ( inject, caseCxt'', Sum, All, (:&:)(..), DistAnn )
import Data.Comp.Trans ( runCompTrans, withSubstitutions, withAnnotationProp, defaultUnpropAnn, deriveTrans, deriveUntrans )

import Cubix.Language.Info
import Cubix.Language.C.Parametric.Full.Names
import Cubix.Language.C.Parametric.Full.Types
import Cubix.Language.Parametric.Syntax.Base
import Cubix.Language.Parametric.Syntax.Functor


do substs <- makeSubsts
   runCompTrans $ withAnnotationProp annType isAnn propAnn defaultUnpropAnn
                $ withSubstitutions substs
                $ deriveTrans origASTTypes annotatedTargType

translate :: C.CTranslationUnit (Maybe SourceSpan) -> CTermOptAnn SourceSpan CTranslationUnitL
translate = trans

translateExtDecl :: C.CExternalDeclaration (Maybe SourceSpan) -> CTermOptAnn SourceSpan CExternalDeclarationL
translateExtDecl = trans

translateStatement :: C.CStatement (Maybe SourceSpan) -> CTermOptAnn SourceSpan CStatementL
translateStatement = trans

translateExpression :: C.CExpression (Maybe SourceSpan) -> CTermOptAnn SourceSpan CExpressionL
translateExpression = trans

translateNodeInfo :: C.NodeInfo -> CTermOptAnn SourceSpan NodeInfoL
translateNodeInfo = trans

instance (Trans c l, Typeable l) => Trans [c] [l] where
  trans [] = inject NilF
  trans (x:xs) = inject $ ConsF (trans x) (trans xs)

instance (Trans c l, Typeable l) => Trans (Maybe c) (Maybe l) where
  trans Nothing  = inject NothingF
  trans (Just x) = inject $ JustF $ trans x

instance (Trans c l, Trans d l', Typeable l, Typeable l') => Trans (c, d) (l, l')  where
  trans (x, y) = inject $ PairF (trans x) (trans y)

instance (Trans c l, Trans d l', Trans e l'', Typeable l, Typeable l', Typeable l'') => Trans (c, d, e) (l, l', l'') where
  trans (x, y, z) = inject $ TripleF (trans x) (trans y) (trans z)

instance (Trans c l, Trans d l', Typeable l, Typeable l') => Trans (Either c d) (Either l l') where
  trans (Left x)  = inject $ LeftF (trans x)
  trans (Right x) = inject $ RightF (trans x)

instance Trans Bool BoolL where
  trans x = inject $ BoolF x

instance Trans Int IntL where
  trans x = inject $ IntF x

instance Trans Integer IntegerL where
  trans x = inject $ IntegerF x

instance Trans () () where
  trans _ = inject UnitF


do substs <- makeSubsts
   runCompTrans $ withAnnotationProp annType isAnn propAnn defaultUnpropAnn
                $ withSubstitutions substs
                $ deriveUntrans origASTTypes annotatedTargType

type instance Targ [l] = [Targ l]
instance Untrans (ListF :&: a) where
  untrans (NilF :&: _) = T []
  untrans (ConsF a b :&: _) = T ((t a) : (t b))

type instance Targ (Maybe l) = Maybe (Targ l)
instance Untrans (MaybeF :&: a) where
  untrans (NothingF :&: _) = T Nothing
  untrans (JustF x  :&: _) = T (Just (t x))

type instance Targ (l, l') = (Targ l, Targ l')
instance Untrans (PairF :&: a) where
  untrans (PairF x y :&: _) = T (t x, t y)

type instance Targ (l, l', l'') = (Targ l, Targ l', Targ l'')
instance Untrans (TripleF :&: a) where
  untrans (TripleF x y z :&: _) = T (t x, t y, t z)

type instance Targ (Either l l') = Either (Targ l) (Targ l')
instance Untrans (EitherF :&: a) where
  untrans (LeftF x  :&: _) = T (Left (t x))
  untrans (RightF x :&: _) = T (Right (t x))

type instance Targ BoolL = Bool
instance Untrans (BoolF :&: a) where
  untrans (BoolF x :&: _) = T x

type instance Targ IntL = Int
instance Untrans (IntF :&: a) where
  untrans (IntF x :&: _) = T x

type instance Targ IntegerL = Integer
instance Untrans (IntegerF :&: a) where
  untrans (IntegerF x :&: _) = T x

type instance Targ () = ()
instance Untrans (UnitF :&: a) where
  untrans (UnitF :&: _) = T ()

instance (All Untrans (DistAnn fs a)) => Untrans (Sum fs :&: a) where
  untrans = caseCxt'' @Untrans untrans

type instance Targ IntL = Int
type instance Targ BoolL = Bool
type instance Targ () = ()
#endif
