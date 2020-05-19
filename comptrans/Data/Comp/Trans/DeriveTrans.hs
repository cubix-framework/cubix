{-# LANGUAGE TemplateHaskell #-}

module Data.Comp.Trans.DeriveTrans
  (
    deriveTrans
  ) where

import Control.Monad.Trans ( lift )
import Data.Map ( Map )

import Control.Lens ( (^.), _1, _2, (%~), view )
import Language.Haskell.TH

import Data.Comp.Multi ( inj, Cxt(Term), (:&:)(..) )

import Data.Comp.Trans.Util

-- |
-- Creates a functions translating from an ADT
-- to its isomorphic multi-sorted compositional data type
-- 
-- @
-- import qualified Foo as F
-- ...
-- type ArithTerm = Term Arith
-- runCompTrans $ deriveTrans [''Arith, ''Atom, ''Lit] (TH.ConT ''ArithTerm)
-- @
-- 
-- will create,
-- 
-- @
-- 
-- class Trans a l where
--   trans :: a -> ArithTerm l
-- 
-- instance Trans F.Arith ArithL where
--   trans (F.Add x y) = iAdd (trans x) (trans y)
-- 
-- instance Trans F.Atom AtomL where
--   trans (F.Var s)   = iVar s
--   trans (F.Const x) = iConst (trans x)
-- 
-- instance Trans F.Lit LitL where
--   trans (F.Lit n) = iLit n
-- @
--
-- With annotation propagation on, it will instead produce, e.g.:
-- `trans :: F.Arith Ann -> Term (Arith :&: Ann) ArithL`
deriveTrans :: [Name] -> Type -> CompTrans [Dec]
deriveTrans names term = do
  let classNm = mkName "Trans"
  funNm <- lift $ newName "trans"

  classDec <- mkClass classNm funNm term

  annPropInf <- view annotationProp
  transAlts <- case annPropInf of
    Just api -> mkAnnotationPropTransAlts api
    Nothing  -> mkNormalTransAlts

  instances <- mapM (mkInstance transAlts classNm funNm) names
  return $ [classDec] ++ instances

data TransAlts = TransAlts {
                             makeTransRhs :: Name -> Name -> [(Name, Type)] -> Body -- Fun nm, constructor, variables, types
                           }

mkAnnotationPropTransAlts :: AnnotationPropInfo -> CompTrans TransAlts
mkAnnotationPropTransAlts api = do substs <- view substitutions
                                   return $ TransAlts { makeTransRhs = makeTransRhsPropAnn substs api}

mkNormalTransAlts :: CompTrans TransAlts
mkNormalTransAlts = return $ TransAlts { makeTransRhs = makeTransRhsNormal}

-- |
-- Example:
-- 
-- @
-- translate :: J.CompilationUnit -> JavaTerm CompilationUnitL
-- translate = trans
-- @
mkFunc :: Type -> Name -> Type -> CompTrans [Dec]
mkFunc typ funNm term = do
  srcTyp <- applyCurSubstitutions typ
  isAnn <- getIsAnn
  lab <- getLab isAnn srcTyp
  return [ SigD translate (AppT (AppT ArrowT srcTyp) (AppT term lab))
         , ValD (VarP translate) (NormalB funNm') []
         ]
  where
    translate = mkName "translate"
    funNm' = VarE funNm

-- |
-- Example:
-- 
-- @
-- class Trans a l where
--   trans a -> JavaTerm l
-- @
mkClass :: Name -> Name -> Type -> CompTrans Dec
mkClass classNm funNm term = do a <- lift $ newName "a"
                                i <- lift $ newName "i"
                                let transDec = SigD funNm (foldl AppT ArrowT [VarT a, AppT term (VarT i)])
                                return $ ClassD [] classNm [PlainTV a, PlainTV i] [] [transDec]

-- |
-- Example:
-- 
-- @
-- instance Trans J.CompilationUnit CompilationUnitL where
--   trans (J.CompilationUnit x y z) = iCompilationUnit (trans x) (trans y) (trans z)
-- @
mkInstance :: TransAlts -> Name -> Name -> Name -> CompTrans Dec
mkInstance transAlts classNm funNm typNm = do
  inf <- lift $ reify typNm
  srcTyp <- getFullyAppliedType typNm
  let nmTyps = simplifyDataInf inf
  clauses <- mapM (uncurry $ mkClause transAlts funNm) nmTyps
  let targNm = nameLab typNm
  return (InstanceD
                   Nothing
                   []
                   (AppT (AppT (ConT classNm) srcTyp) (ConT targNm))
                   [FunD funNm clauses])


atom :: Name -> (Name, Type) -> Exp
atom _     (x, t) | elem t baseTypes = VarE x
atom funNm (x, _)                    = AppE (VarE funNm) (VarE x)

makeTransRhsPropAnn :: Map Name Type -> AnnotationPropInfo -> Name -> Name -> [(Name, Type)] -> Body
makeTransRhsPropAnn substs annPropInf funNm con nmTps = NormalB $ AppE (ConE 'Term) $ AppE (AppE (ConE '(:&:)) nodeExp) annExp
  where
    nmTps' :: [(Name, Type)]
    nmTps' = map (_2 %~ (applySubsts substs)) nmTps

    annVar :: (a, Type) -> Bool
    annVar (_, t) = (annPropInf ^. isAnnotation) t

    nodeExp = AppE (VarE 'inj) $ foldl AppE (ConE (transName con)) (map (atom funNm) $ filter (not.annVar) nmTps')

    annExp = (annPropInf ^. propAnn) (map (_1 %~ VarE) $ filter annVar nmTps')

makeTransRhsNormal :: Name -> Name -> [(Name, Type)] -> Body
makeTransRhsNormal funNm con nmTps = NormalB $ foldl AppE (VarE (smartConstrName con)) (map (atom funNm) nmTps)

mkClause :: TransAlts -> Name -> Name -> [Type] -> CompTrans Clause
mkClause transAlts funNm con tps = do nms <- lift $ mapM (const $ newName "x") tps
                                      return $ Clause [pat nms] (makeTransRhs transAlts funNm con $ zip nms tps) []
  where
    pat nms = ConP con (map VarP nms)
