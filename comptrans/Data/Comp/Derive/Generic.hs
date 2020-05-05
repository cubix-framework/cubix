{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Allows you to derive instances of GHC.Generics for compositional data types.
-- Warning: May slaughter your compile times.

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-} -- TH runs at compile time, so you get compile-time errors anyway
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-} -- It warns for the instance declarations in TH which are never directly compiled -- GAH

module Data.Comp.Derive.Generic
  (
    makeGeneric
  , makeInstancesLike
  , GenericExample
  ) where

import Control.Lens ( (%~), (&), traversed )
import Control.Monad ( liftM, filterM, mplus, msum )

import qualified Data.Comp.Multi as M
import qualified Data.Comp.Multi.Ops as M

import GHC.Generics ( Generic(..), (:*:)(..), (:+:)(..), K1(..), V1, Rec0, U1(..) )

import Language.Haskell.TH
import Data.Type.Equality

import Data.Comp.Trans.Util

--------------------------------------------------------------------------------
-- Generic instances for general CDTs
--------------------------------------------------------------------------------
instance Generic (M.Sum '[] e l) where
  type Rep (M.Sum '[] e l) = V1

instance (Generic (f e l), Generic (M.Sum fs e l)) => Generic (M.Sum (f ': fs) e l) where
  type Rep (M.Sum (f ': fs) e l) = Rep (f e l) :+: Rep (M.Sum fs e l)

  from (M.Sum w a) = case M.contract w of
    Right w0 -> R1 (from (M.Sum w0 a))
    Left Refl -> L1 (from a)

  to (L1 x) = M.Sum M.witness (to x :: f e l)
  to (R1 x) = case to x :: M.Sum fs e l of
    M.Sum w a -> M.Sum (M.extend w) a

instance (Generic (f (M.HFix f) l)) => Generic (M.HFix f l) where
  type Rep (M.HFix f l) = Rep (f (M.HFix f) l)
  from (M.Term x) = from x
  to x = M.Term $ to x

instance (Generic (f e l)) => Generic ((f M.:&: p) e l) where
  type Rep ((f M.:&: p) e l) = (Rep (f e l)) :*: Rec0 p
  from (t M.:&: x) = from t :*: K1 x
  to (t :*: K1 x) = to t M.:&: x

--------------------------------------------------------------------------------
-- Creating users of Generic
--------------------------------------------------------------------------------

data GenericExample

makeInstancesLike :: [Name] -> [Type] -> Q [Dec] -> Q [Dec]
makeInstancesLike cons labs example = do
  [InstanceD ov [] (AppT (ConT tc) _) b] <- example
  return [makeInstanceLike ov tc c l b | c <- cons, l <- labs]

makeInstanceLike :: Maybe Overlap -> Name -> Name -> Type -> [Dec] -> Dec
makeInstanceLike ov tc c l b =
  InstanceD ov [] (AppT (ConT tc) (AppT (ConT c) l)) b

--------------------------------------------------------------------------------
-- Deriving Generic
--------------------------------------------------------------------------------

makeGeneric :: [Name] -> [Type] -> Q [Dec]
makeGeneric nms tps = liftM concat $ sequence [makeGenericInstance n t | n <- nms, t <- tps]

makeGenericInstance :: Name -> Type -> Q [Dec]
makeGenericInstance typNm lab = do
    cons <- liftM simplifyDataInf $ reify typNm
    relCons <- filterM (matchingCon lab . fst) cons
    let mTyp = conT typNm
    let mLab = return lab

    case relCons of
      [] -> [d| instance Generic ($mTyp e $mLab) where
                  type Rep ($mTyp e $mLab) = V1
                  from = undefined
                  to = undefined
              |]

      xs -> do let xts = map snd xs


               vars1 <- mapM (mapM (const $ newName "x")) xts
               vars2 <- mapM (mapM (const $ newName "x")) xts
               eNm   <- case msum $ map (msum.map getEVar) $ map snd xs of
                          Just n  -> return n
                          Nothing -> newName "e"
               
               let e   = return (VarT eNm)
               let rep = return $ genericTp xts


               let gPat = addSumPat $ map makeGPat $ vars1
               let gExp = addSumExp $ map makeGExp $ vars2
               let ePat = map makeEPat $ zip xs vars2 & traversed %~ (\((n,_),ns) -> (n, ns))
               let eExp = map makeEExp $ zip xs vars1 & traversed %~ (\((n,_),ns) -> (n, ns))

               inst' <- one [d| instance Generic ($mTyp $e $mLab) where
                                  type Rep ($mTyp $e $mLab) = $rep
                              |]

               addDecs inst' $
                   [ FunD 'from (map mkClause $ zip ePat gExp)
                   , FunD 'to   (map mkClause $ zip gPat eExp)
                   ]
             where
               one = liftM head
               addDecs (InstanceD ov c t ds) ds' = return $ [InstanceD ov c t (ds++ds')]
               mkClause (pat, expr) = Clause [pat] (NormalB expr) []

               getEVar (AppT (VarT n) _) = Just n
               getEVar (AppT x y )       = getEVar x `mplus` getEVar y
               getEVar _                 = Nothing
           

genericTp :: [[Type]] -> Type
genericTp ts = combine ''(:+:) $ map (combine ''(:*:)) $ map (map (AppT (ConT ''Rec0))) ts
  where
    combine _ []     = ConT ''U1
    combine _ [x]    = x
    combine c (x:xs) = AppT (AppT (ConT c) x) (combine c xs)

makeGPat :: [Name] -> Pat
makeGPat []     = ConP 'U1 []
makeGPat [n]    = ConP 'K1 [VarP n]
makeGPat (n:ns) = ConP '(:*:) [ ConP 'K1 [VarP n]
                              , makeGPat ns 
                              ]

makeGExp :: [Name] -> Exp
makeGExp []     = ConE 'U1
makeGExp [n]    = AppE (ConE 'K1) (VarE n)
makeGExp (n:ns) = AppE (AppE (ConE '(:*:)) (AppE (ConE 'K1) (VarE n))) (makeGExp ns) 

makeEPat :: (Name, [Name]) -> Pat
makeEPat (c, ns) = ConP c (map VarP ns)

makeEExp :: (Name, [Name]) -> Exp
makeEExp (c, ns) = foldl AppE (ConE c) (map VarE ns)

addSumPat :: [Pat] -> [Pat]
addSumPat [p]    = [p]
addSumPat (p:ps) = [ConP 'L1 [p]] ++ map (\r -> ConP 'R1 [r]) (addSumPat ps)

addSumExp :: [Exp] -> [Exp]
addSumExp [e]    = [e]
addSumExp (e:es) = [AppE (ConE 'L1) e] ++ map (\f -> AppE (ConE 'R1) f) (addSumExp es)

matchingCon :: Type -> Name -> Q Bool
matchingCon t nm = do
  (DataConI _ tp parentNm) <- reify nm
  return $ cxtlessUnifiable (extractLab tp parentNm) t


extractLab :: Type -> Name -> Type
extractLab tp par = go tp
  where
    go (ForallT _ ctx t)      = go $ substCxt ctx t
    go (AppT (AppT (ConT n) _) t)
                 | par == n = t
    go (AppT _ t)           = go t

    -- My very ghetto way of handling contexts. Found a few
    -- examples where GHC substituted away equality constraints
    -- when getting the type of a data con; assumed it always did,
    -- and now paying the price.
    substCxt [] t                                        = t
    substCxt (AppT (AppT EqualityT (VarT n)) t' : ctx) t = substCxt ctx (tsubst t' n t)
    substCxt (AppT (AppT EqualityT t') (VarT n) : ctx) t = substCxt ctx (tsubst t' n t)
    substCxt (_ : ctx) t                                 = substCxt ctx t
    
    tsubst t n (AppT l r) = AppT (tsubst t n l) (tsubst t n r)
    tsubst t n (VarT n')
                | n == n' = t
    tsubst _ _ x          = x
      
cxtlessUnifiable :: Type -> Type -> Bool
cxtlessUnifiable t u | t == u = True
cxtlessUnifiable (VarT _) _   = True
cxtlessUnifiable _ (VarT _)   = True
cxtlessUnifiable (AppT t1 u1)
                 (AppT t2 u2) = (cxtlessUnifiable t1 t2) && (cxtlessUnifiable u1 u2)
cxtlessUnifiable _ _          = False
