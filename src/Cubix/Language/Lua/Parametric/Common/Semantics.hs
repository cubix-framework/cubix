{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cubix.Language.Lua.Parametric.Common.Semantics () where

import Data.Type.Equality ( (:~:)(..), gcastWith )
import Data.Proxy ( Proxy(..) )

import Data.Comp.Multi ( (:<:), project, inject' )
import Data.Comp.Multi.Strategy.Classification ( KDynCase(..), kIsSort )

import Cubix.Language.Lua.Parametric.Common.Types
import Cubix.Language.Lua.Parametric.Full
import Cubix.Language.Parametric.Semantics.SemanticProperties
import Cubix.Language.Parametric.Syntax

import Cubix.Sin.Compdata.Annotation ( annM )

instance {-# OVERLAPPING #-} (Binop :<: g) => GetStrictness' g Exp where
  getStrictness' t@(Binop op _ _) = case project op of
    Just And -> [NoEval, Strict, GuardedBy (Place 1)]
    Just Or  -> [NoEval, Strict, GuardedBy (NegPlace 1)]
    _           -> defaultGetStrictness t
  getStrictness' x                  = defaultGetStrictness x

-- Special casing functions so we don't put stuff at top level b/c looks weird even though correct
instance {-# OVERLAPPING #-} InsertAt' ListF MLuaSig BlockItemL where
  insertAt' EnterEvalPoint e t = case kdyncase t :: Maybe (_ :~: [BlockItemL]) of
                                     Nothing -> return $ inject' t
                                     Just p  -> gcastWith p $ inject' <$> annM (ConsF e (inject' t))
  insertAt' _              _ t = return $ inject' t

  canInsertAt' EnterEvalPoint _ = kIsSort (Proxy :: Proxy [BlockItemL])
  canInsertAt' _              _ = const False
