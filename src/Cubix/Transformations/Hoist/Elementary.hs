{-# OPTIONS_HADDOCK hide #-}

-- | A simplified version of hoist for use in examples

module Cubix.Transformations.Hoist.Elementary (
    elementaryHoist
  ) where

import Data.Comp.Multi ( project, transform, HTraversable, Term, (:-<:), All )

import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax
import Cubix.Transformations.Hoist.Custom


type VarDeclFragment fs = ( SingleLocalVarDecl :-<: fs, MultiLocalVarDecl :-<: fs
                         , OptLocalVarInit :-<: fs, Ident :-<: fs )
type AssignFragment fs = ( Assign :-<: fs, AssignOpEquals :-<: fs )
type HasFunctors fs = (ListF :-<: fs, ExtractF [] (Term fs))
type CanHoist fs = ( VarDeclFragment fs, AssignFragment fs, Block :-<: fs
                   , VarInitToRhs (Term fs), VarDeclBinderToLhs (Term fs)
                   , InjF fs MultiLocalVarDeclL BlockItemL
                   , InjF fs AssignL BlockItemL
                   , All HTraversable fs, HasFunctors fs
                   )

declToAssign :: (CanHoist fs) => Term fs MultiLocalVarDeclCommonAttrsL -> Term fs SingleLocalVarDeclL -> [Term fs BlockItemL]
declToAssign mattrs (SingleLocalVarDecl' lattrs b optInit) = case optInit of
  NoLocalVarInit'        -> []
  JustLocalVarInit' init -> [injF (Assign' (varDeclBinderToLhs b)  AssignOpEquals' (varInitToRhs mattrs b lattrs init))]

removeInit :: (CanHoist fs) => Term fs SingleLocalVarDeclL -> Term fs SingleLocalVarDeclL
removeInit (SingleLocalVarDecl' a n _) = SingleLocalVarDecl' a n NoLocalVarInit'

splitDecl :: (CanHoist fs) => Term fs BlockItemL -> ([Term fs BlockItemL], [Term fs BlockItemL])
splitDecl (projF -> (Just (MultiLocalVarDecl' attrs decls))) = ([injF (MultiLocalVarDecl' attrs (mapF removeInit decls))],
                                                                 concat (map (declToAssign attrs) (extractF decls)))
splitDecl t                                                  = ([], [t])

hoistBlockItems :: (CanHoist fs) => [Term fs BlockItemL] -> [Term fs BlockItemL]
hoistBlockItems bs = concat decls ++ concat stmts
  where (decls, stmts) = unzip (map splitDecl bs)

elementaryHoist :: (CanHoist fs) => Term fs l -> Term fs l
elementaryHoist t = transform hoistInner t
  where hoistInner :: (CanHoist fs) => Term fs l -> Term fs l
        hoistInner (project -> (Just (Block bs e))) = Block' (liftF hoistBlockItems bs) e
        hoistInner t                                = t
