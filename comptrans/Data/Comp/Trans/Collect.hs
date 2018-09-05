{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Comp.Trans.Collect (
    collectTypes
  ) where

import Control.Lens ( view )
import Control.Monad ( liftM, liftM2 )
import Control.Monad.Trans ( lift )

import Data.Foldable ( fold )

import Data.Set as Set ( Set, singleton, union, difference, toList, member, empty )

import Language.Haskell.TH.Syntax hiding ( lift )
import Language.Haskell.TH.ExpandSyns ( expandSyns )

import Data.Comp.Trans.Util

-- | Finds all type names transitively referred to by a given type,
-- removing standard types
collectTypes :: Name -> CompTrans [Name]
collectTypes n = do names <- fixpoint collectTypes' n
                    exclNms <- view excludedNames
                    return $ toList $ difference names exclNms

-- |
-- Finds the fixpoint of a monotone monadic function using chaotic iteration
fixpoint :: (Ord a, Monad m) => (a -> m (Set a)) -> a -> m (Set a)
fixpoint f x = run $ singleton x
  where
    run s = do s' <- liftM fold $ mapSetM f s
               if s' == s then
                 return s'
                else
                 run s'

-- | mapM for Data.Set
mapSetM :: (Monad m, Ord b) => (a -> m b) -> Set a -> m (Set b)
mapSetM f x = liftM (mconcat . map singleton) $ mapM f (toList x)

collectTypes' :: Name -> CompTrans (Set Name)
collectTypes' n = view excludedNames >>= run
  where
    run :: Set Name -> CompTrans (Set Name)
    run exclNms | member n exclNms = return empty
    run _                          = do
      inf <- lift $ reify n
      let cons = case inf of
            TyConI (DataD _ _ _ cns _)    -> cns
            TyConI (NewtypeD _ _ _ con _) -> [con]
            _ -> []
      childNames <- liftM concat $ mapM extractNames cons
      return $ (singleton n) `union` (mconcat $ map singleton childNames)
                    

class ExtractNames a where
  extractNames :: a -> CompTrans [Name]

instance ExtractNames Con where
  extractNames (NormalC _ xs) = liftM concat $ mapM extractNames xs
  extractNames (RecC _ xs) = liftM concat $ mapM extractNames xs
  extractNames (InfixC a _ b) = liftM2 (++) (extractNames a) (extractNames b)
  extractNames (ForallC _ _ x) = extractNames x

instance ExtractNames StrictType where
  extractNames (_, t) = extractNames t

instance ExtractNames VarStrictType where
  extractNames (_, _, t) = extractNames t

instance ExtractNames Type where
  extractNames tSyn = do t <- lift $ expandSyns tSyn
                         case t of 
                           AppT a b -> liftM2 (++) (extractNames a) (extractNames b)
                           ConT n   -> return [n]
                           _        -> return []
