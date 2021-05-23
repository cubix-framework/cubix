{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE TemplateHaskell #-}

module Cubix.Transformations.TAC.Gensym (
    GensymState(..)
  , HasGensymState(..)

  , gensym
  , finalizeGensymNames
 ) where

import Control.Monad.State ( MonadState )

import Data.List ( isPrefixOf )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set

import Control.Lens ( (+=), (%=), use, makeClassy )

import Data.Comp.Multi ( Cxt(..), (:&:)(..), (:-<:), project', inj, All, TreeLike )
import Data.Comp.Multi.Strategic ( GRewriteM, RewriteM, promoteRF, anytdR, tryR )
import Data.Comp.Multi.Strategy.Classification ( DynCase )

import Cubix.Language.Info
import Cubix.Language.Parametric.Syntax

import Cubix.Sin.Compdata.Annotation ( MonadAnnotater, getAnn )

-- | Easiest way to get proper label generation:
-- Do transform in whatever order is convenient, give placeholder names for temporaries,
-- then do a second pass and provide the real temporary names
--
-- Ideally this would be done using Hole's instead of placeholder names,
-- but this was already set up to be done on Term's rather than Context's, and deadlines
--
-- Also, it was kinda crappy to rewrite the entire TAC to use context's

data GensymState = GensymState { _gensymCounter :: Int
                               , _allIdents     :: Set String
                               , _gensymNameMap :: Map String String
                               }

makeClassy ''GensymState

gensymPrefix :: String
gensymPrefix = "i_am_a_temp_replace_me"

tmpVarPrefix :: String
tmpVarPrefix = "t"

gensym :: (MonadState s m, HasGensymState s, MonadAnnotater Label m, Ident :-<: fs) => m (TermLab fs IdentL)
gensym = do
  gensymCounter += 1
  n <- use gensymCounter
  prevIds <- use allIdents
  let id = gensymPrefix ++ show n
  if Set.member id prevIds then
    gensym
   else
    annotateTop' (Ident id)

finalizeGensymName :: (MonadState s m, HasGensymState s, TreeLike fs, Ident :-<: fs) => RewriteM m (TermLab fs) IdentL
finalizeGensymName t@(project' -> Just (Ident s)) =
                                  let lab = getAnn t in
                                  if isPrefixOf gensymPrefix s then do
                                    nameMap <- use gensymNameMap
                                    case Map.lookup s nameMap of
                                      Just nm -> return $ Term ((inj $ Ident nm) :&: lab)
                                      Nothing -> do
                                        gensymCounter += 1
                                        n <- use gensymCounter
                                        prevIds <- use allIdents
                                        let id = tmpVarPrefix ++ show n
                                        if Set.member id prevIds then
                                          finalizeGensymName t
                                         else do
                                           gensymNameMap %= Map.insert s id
                                           return $ Term $ (inj $ Ident id) :&: lab

                                  else
                                    return t

finalizeGensymNames ::
  ( MonadState s m
  , HasGensymState s
  , TreeLike fs
  , DynCase (TermLab fs) IdentL
  , Ident :-<: fs
  ) => GRewriteM m (TermLab fs)
finalizeGensymNames = tryR $ anytdR $ promoteRF finalizeGensymName
