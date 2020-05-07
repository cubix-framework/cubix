-- | A simplified version of hoist for use in examples

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Cubix.Transformations.Hoist.Elementary (
    elementaryHoist
  ) where

import Data.Comp.Multi ( (:<:), HFix, project, transform, HTraversable )

import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax
import Cubix.Transformations.Hoist.Custom


type VarDeclFragment f = ( SingleLocalVarDecl :<: f, MultiLocalVarDecl :<: f
                         , OptLocalVarInit :<: f, Ident :<: f )
type AssignFragment f = ( Assign :<: f, AssignOpEquals :<: f )
type HasFunctors f = (ListF :<: f, ExtractF [] (HFix f))
type CanHoist f = ( VarDeclFragment f, AssignFragment f, Block :<: f
                  , VarInitToRhs (HFix f), VarDeclBinderToLhs (HFix f)
                  , InjF f MultiLocalVarDeclL BlockItemL
                  , InjF f AssignL BlockItemL
                  , HTraversable f, HasFunctors f
                  )

declToAssign :: (CanHoist f) => HFix f MultiLocalVarDeclCommonAttrsL -> HFix f SingleLocalVarDeclL -> [HFix f BlockItemL]
declToAssign mattrs (SingleLocalVarDecl' lattrs b optInit) = case optInit of
  NoLocalVarInit'        -> []
  JustLocalVarInit' init -> [injF (Assign' (varDeclBinderToLhs b)  AssignOpEquals' (varInitToRhs mattrs b lattrs init))]

removeInit :: (CanHoist f) => HFix f SingleLocalVarDeclL -> HFix f SingleLocalVarDeclL
removeInit (SingleLocalVarDecl' a n _) = SingleLocalVarDecl' a n NoLocalVarInit'

splitDecl :: (CanHoist f) => HFix f BlockItemL -> ([HFix f BlockItemL], [HFix f BlockItemL])
splitDecl (projF -> (Just (MultiLocalVarDecl' attrs decls))) = ([injF (MultiLocalVarDecl' attrs (mapF removeInit decls))],
                                                                 concat (map (declToAssign attrs) (extractF decls)))
splitDecl t                                                  = ([], [t])

hoistBlockItems :: (CanHoist f) => [HFix f BlockItemL] -> [HFix f BlockItemL]
hoistBlockItems bs = concat decls ++ concat stmts
  where (decls, stmts) = unzip (map splitDecl bs)

elementaryHoist :: (CanHoist f) => HFix f l -> HFix f l
elementaryHoist t = transform hoistInner t
  where hoistInner :: (CanHoist f) => HFix f l -> HFix f l
        hoistInner (project -> (Just (Block bs e))) = Block' (liftF hoistBlockItems bs) e
        hoistInner t                                = t
