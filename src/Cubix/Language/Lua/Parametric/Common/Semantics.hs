{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances  #-}

module Cubix.Language.Lua.Parametric.Common.Semantics () where

import Data.Type.Equality ( (:~:)(..), gcastWith )

import Data.Comp.Multi ( (:-<:), project, inject')
import Data.Comp.Multi.Strategy.Classification ( KDynCase(..), kIsSort )

import Cubix.Language.Lua.Parametric.Common.Types
import Cubix.Language.Lua.Parametric.Full
import Cubix.Language.Parametric.Semantics.SemanticProperties
import Cubix.Language.Parametric.Syntax

import Cubix.Sin.Compdata.Annotation ( annM )

instance {-# OVERLAPPING #-} (Binop :-<: gs) => GetStrictness' gs Exp where
  getStrictness' t@(Binop op _ _) = case op of
    And' -> [NoEval, Strict, GuardedBy (Place 1)]
    Or'  -> [NoEval, Strict, GuardedBy (NegPlace 1)]
    _    -> defaultGetStrictness t
  getStrictness' x                  = defaultGetStrictness x

-- Special casing functions so we don't put stuff at top level b/c looks weird even though correct
instance {-# OVERLAPPING #-} InsertAt' MLuaSig BlockItemL ListF where
  insertAt' EnterEvalPoint e t = case kdyncase t :: Maybe (_ :~: [BlockItemL]) of
                                     Nothing -> return $ inject' t
                                     Just p  -> gcastWith p $ inject' <$> annM (ConsF e (inject' t))
  insertAt' _              _ t = return $ inject' t

  canInsertAt' EnterEvalPoint _ = kIsSort @[BlockItemL]
  canInsertAt' _              _ = const False
