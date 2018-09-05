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

import Data.Comp.Multi ( (:<:), Term, project, transform, HTraversable )

import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax
import Cubix.Transformations.Hoist.Custom


type VarDeclFragment f = ( SingleLocalVarDecl :<: f, MultiLocalVarDecl :<: f
                         , OptLocalVarInit :<: f, Ident :<: f )
type AssignFragment f = ( Assign :<: f, AssignOpEquals :<: f )
type HasFunctors f = (ListF :<: f, ExtractF [] (Term f))
type CanHoist f = ( VarDeclFragment f, AssignFragment f, Block :<: f
                  , VarInitToRhs (Term f), VarDeclBinderToLhs (Term f)
                  , InjF f MultiLocalVarDeclL BlockItemL
                  , InjF f AssignL BlockItemL
                  , HTraversable f, HasFunctors f
                  )

declToAssign :: (CanHoist f) => Term f MultiLocalVarDeclCommonAttrsL -> Term f SingleLocalVarDeclL -> [Term f BlockItemL]
declToAssign mattrs (SingleLocalVarDecl' lattrs b optInit) = case optInit of
  NoLocalVarInit'        -> []
  JustLocalVarInit' init -> [injF (Assign' (varDeclBinderToLhs b)  AssignOpEquals' (varInitToRhs mattrs b lattrs init))]

removeInit :: (CanHoist f) => Term f SingleLocalVarDeclL -> Term f SingleLocalVarDeclL
removeInit (SingleLocalVarDecl' a n _) = SingleLocalVarDecl' a n NoLocalVarInit'

splitDecl :: (CanHoist f) => Term f BlockItemL -> ([Term f BlockItemL], [Term f BlockItemL])
splitDecl (projF -> (Just (MultiLocalVarDecl' attrs decls))) = ([injF (MultiLocalVarDecl' attrs (mapF removeInit decls))],
                                                                 concat (map (declToAssign attrs) (extractF decls)))
splitDecl t                                                  = ([], [t])

hoistBlockItems :: (CanHoist f) => [Term f BlockItemL] -> [Term f BlockItemL]
hoistBlockItems bs = concat decls ++ concat stmts
  where (decls, stmts) = unzip (map splitDecl bs)

elementaryHoist :: (CanHoist f) => Term f l -> Term f l
elementaryHoist t = transform hoistInner t
  where hoistInner :: (CanHoist f) => Term f l -> Term f l
        hoistInner (project -> (Just (Block bs e))) = Block' (liftF hoistBlockItems bs) e
        hoistInner t                                = t
