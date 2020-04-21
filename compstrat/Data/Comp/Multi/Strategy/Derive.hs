{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP             #-}

module Data.Comp.Multi.Strategy.Derive (
    makeDynCase
  ) where

import Control.Arrow ( (&&&) )
import Control.Monad

import Data.List ( nub )
import Data.Maybe ( catMaybes )
import Data.Type.Equality ( (:~:)(..) )

import Language.Haskell.TH hiding ( Cxt )

import Data.Comp.Multi.Strategy.Classification ( KDynCase, kdyncase )


makeDynCase :: Name -> Q [Dec]
makeDynCase fname = do
          -- TODO: replace abstractNewtypeQ with generic definitionn from Data.Comp.Derive.Utils
          TyConI (DataD _cxt tname targs _ constrs _deriving) <- abstractNewtypeQ $ reify fname
          let iVar = tyVarBndrName $ last targs
          let labs = nub $ catMaybes $ map (iTp iVar) constrs
          let cons = map (abstractConType &&& iTp iVar) constrs
          mapM (genDyn tname cons) labs
     where
       iTp :: Name -> Con -> Maybe Type
       iTp iVar (ForallC _ cxt _) =
         -- Check if the GADT phantom type is constrained
         case [y | AppT (AppT EqualityT x) y <- cxt, x == VarT iVar] of
           [] -> Nothing
           tp:_ -> Just tp
       iTp _ _ = Nothing
  
       genDyn :: Name -> [((Name, Int), Maybe Type)] -> Type -> Q Dec
       genDyn tname cons tp = do
           clauses <- liftM concat $ mapM (mkClause tp) cons
           let body = [FunD 'kdyncase clauses]
           instTp  <- forallT []
                              (return [])
                              (foldl appT (conT ''KDynCase) [conT tname, return tp])
           return $ InstanceD Nothing [] instTp body
  
       mkClause :: Type -> ((Name, Int), Maybe Type) -> Q [Clause]
       mkClause tp (con, Just tp')
                   | tp == tp' = return [Clause [conPat con] 
                                                (NormalB (AppE (ConE 'Just) (ConE 'Refl)))
                                                []]
       mkClause _ (con, _) = return [Clause [conPat con]
                                            (NormalB (ConE 'Nothing))
                                            []]
  
       conPat :: (Name, Int) -> Pat
       conPat (con, n) = ConP con (replicate n WildP)


{-|
  This is the @Q@-lifted version of 'abstractNewtypeQ.
-}
abstractNewtypeQ :: Q Info -> Q Info
abstractNewtypeQ = liftM abstractNewtype

{-|
  This function abstracts away @newtype@ declaration, it turns them into
  @data@ declarations.
-}
abstractNewtype :: Info -> Info
abstractNewtype (TyConI (NewtypeD cxt name args mk constr derive))
    = TyConI (DataD cxt name args mk [constr] derive)
abstractNewtype owise = owise


{-|
  This function provides the name and the arity of the given data constructor.
-}
abstractConType :: Con -> (Name,Int)
abstractConType (NormalC constr args) = (constr, length args)
abstractConType (RecC constr args) = (constr, length args)
abstractConType (InfixC _ constr _) = (constr, 2)
abstractConType (ForallC _ _ constr) = abstractConType constr

{-|
  This function returns the name of a bound type variable
-}
tyVarBndrName (PlainTV n) = n
tyVarBndrName (KindedTV n _) = n



{-|
  This function provides a list (of the given length) of new names based
  on the given string.
-}
newNames :: Int -> String -> Q [Name]
newNames n name = replicateM n (newName name)

