{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Multi.Derive.Ordering
-- Copyright   :  (c) 2011 Patrick Bahr, Tom Hvitved
-- License     :  BSD3
-- Maintainer  :  Tom Hvitved <hvitved@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Automatically derive instances of @OrdHF@.
--
--------------------------------------------------------------------------------
module Data.Comp.Multi.Derive.Ordering
    (
     OrdHF(..),
     makeOrdHF
    ) where

import Data.Comp.Derive.Utils
import Data.Comp.Multi.Ordering
import Data.List
import Data.Maybe
import Language.Haskell.TH hiding (Cxt)

compList :: [Ordering] -> Ordering
compList = fromMaybe EQ . find (/= EQ)

{-| Derive an instance of 'OrdHF' for a type constructor of any parametric
  kind taking at least three arguments. -}
makeOrdHF :: Name -> Q [Dec]
makeOrdHF fname = do
  Just (DataInfo _ name args constrs _) <- abstractNewtypeQ $ reify fname
  let args' = init args
  -- covariant argument
  let coArg :: Type = VarT $ tyVarBndrName $ last args'
  let argNames = map (VarT . tyVarBndrName) (init args')
  let complType = foldl AppT (ConT name) argNames
  let classType = AppT (ConT ''OrdHF) complType
  constrs' :: [(Name,[Type],Maybe Type)] <- mapM normalConExp constrs
  compareHFDecl <- funD 'compareHF (compareHFClauses coArg constrs')
  return [mkInstanceD [] classType [compareHFDecl]]
      where compareHFClauses :: Type -> [(Name,[Type],Maybe Type)] -> [ClauseQ]
            compareHFClauses _ [] = []
            compareHFClauses coArg constrs =
                let constrs' = constrs `zip` [1..]
                    constPairs = [(x,y)| x<-constrs', y <- constrs']
                in map (genClause coArg) constPairs
            genClause coArg ((c,n),(d,m))
                | n == m = genEqClause coArg c
                | n < m = genLtClause c d
                | otherwise = genGtClause c d
            genEqClause :: Type -> (Name,[Type],Maybe Type) -> ClauseQ
            genEqClause coArg (constr, args,gadtTy) = do
              varXs <- newNames (length args) "x"
              varYs <- newNames (length args) "y"
              let patX = ConP constr [] $ map VarP varXs
              let patY = ConP constr [] $ map VarP varYs
              body <- eqDBody (getBinaryFArg coArg gadtTy) (zip3 varXs varYs args)
              return $ Clause [patX, patY] (NormalB body) []
            eqDBody :: Type -> [(Name, Name, Type)] -> ExpQ
            eqDBody coArg x =
                [|compList $(listE $ map (eqDB coArg) x)|]
            eqDB :: Type -> (Name, Name, Type) -> ExpQ
            eqDB coArg (x, y, tp)
                | not (containsType tp coArg) =
                    [| compare $(varE x) $(varE y) |]
                | otherwise =
                    [| kcompare $(varE x) $(varE y) |]
            genLtClause (c, _, _) (d, _, _) =
                clause [recP c [], recP d []] (normalB [| LT |]) []
            genGtClause (c, _, _) (d, _, _) =
                clause [recP c [], recP d []] (normalB [| GT |]) []
