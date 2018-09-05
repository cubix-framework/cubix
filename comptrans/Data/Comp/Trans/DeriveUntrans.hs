{-# LANGUAGE TemplateHaskell #-}

module Data.Comp.Trans.DeriveUntrans (
    deriveUntrans
  ) where

import Control.Lens ( view ,(^.))
import Control.Monad ( liftM )
import Control.Monad.Trans ( lift )

import Data.Comp.Multi ( Alg, cata, (:&:)(..) )

import Language.Haskell.TH

import Data.Comp.Trans.Util

--------------------------------------------------------------------------------


-- |
-- Creates an @untranslate@ function inverting the @translate@ function
-- created by @deriveTrans@.
-- 
-- @
-- import qualified Foo as F
-- type ArithTerm = Term (Arith :+: Atom :+: Lit)
-- deriveUntrans [''F.Arith, ''F.Atom, ''F.Lit] (TH.ConT ''ArithTerm)
-- @
-- 
-- will create
-- 
-- @
-- type family Targ l
-- newtype T l = T {t :: Targ l}
-- 
-- class Untrans f where
--   untrans :: Alg f t
-- 
-- untranslate :: ArithTerm l -> Targ l
-- untranslate = t . cata untrans
-- 
-- type instance Targ ArithL = F.Arith
-- instance Untrans Arith where
--   untrans (Add x y) = T $ F.Add (t x) (t y)
-- 
-- type instance Targ AtomL = F.Atom
-- instance Untrans Atom where
--   untrans (Var s)   = T $ F.Var s
--   untrans (Const x) = T $ F.Const (t x)
-- 
-- type instance Targ LitL = F.Lit
-- instance Untrans Lit where
--   untrans (Lit n) = T $ F.Lit n
-- @
-- 
-- Note that you will need to manually provide an instance @(Untrans f, Untrans g) => Untrans (f :+: g)@
-- due to phase issues. (Or @(Untrans (f :&: p), Untrans (g :&: p)) => Untrans ((f :+: g) :&: p)@, if you
-- are propagating annotations.)
--
-- With annotation propagation on, it will instead produce
-- `untranslate :: Term (Arith :&: Ann) l -> Targ l Ann`
deriveUntrans :: [Name] -> Type -> CompTrans [Dec]
deriveUntrans names term = do targDec <- mkTarg targNm
                              wrapperDec <- mkWrapper wrapNm unwrapNm targNm
                              fnDec <- mkFn untranslateNm term targNm unwrapNm fnNm
                              classDec <- mkClass classNm fnNm wrapNm
                              instances <- liftM concat $ mapM (mkInstance classNm fnNm wrapNm unwrapNm targNm) names
                              return $ concat [ targDec
                                              , wrapperDec
                                              , fnDec
                                              , classDec
                                              , instances
                                              ]
  where
    targNm = mkName "Targ"
    wrapNm = mkName "T"
    unwrapNm = mkName "t"
    untranslateNm = mkName "untranslate"
    classNm = mkName "Untrans"
    fnNm = mkName "untrans"

{- type family Targ l -}
mkTarg :: Name -> CompTrans [Dec]
mkTarg targNm = do i <- lift $ newName "i"
                   return [FamilyD TypeFam targNm [PlainTV i] Nothing]

{- newtype T l = T { t :: Targ l } -}
mkWrapper :: Name -> Name -> Name -> CompTrans [Dec]
mkWrapper tpNm fNm targNm = do i <- lift $ newName "i"
                               let con = RecC tpNm [(fNm, NotStrict, AppT (ConT targNm) (VarT i))]
                               return [NewtypeD [] tpNm [PlainTV i] con []]
{-
  untranslate :: JavaTerm l -> Targ l
  untranslate = t . cata untrans
-}
mkFn :: Name -> Type -> Name -> Name -> Name -> CompTrans [Dec]
mkFn fnNm term targNm fldNm untransNm = sequence [sig, def]
  where
    sig = do i <- lift $ newName "i"
             lift $ sigD fnNm (forallT [PlainTV i] (return []) (typ $ varT i))

    typ :: Q Type -> Q Type
    typ i = [t| $term' $i -> $targ $i |]

    term' = return term
    targ = conT targNm

    def = lift $ valD (varP fnNm) (normalB body) []

    body = [| $fld . cata $untrans |]

    fld = varE fldNm
    untrans = varE untransNm

{-
  class Untrans f where
    untrans :: Alg f T
-}
mkClass :: Name -> Name -> Name -> CompTrans [Dec]
mkClass classNm funNm newtpNm = do f <- lift $ newName "f"
                                   let funDec = SigD funNm (AppT (AppT (ConT ''Alg) (VarT f)) (ConT newtpNm))
                                   return [ClassD [] classNm [PlainTV f] [] [funDec]]
                      
{-
  type instance Targ CompilationUnitL = J.CompilationUnit
  instance Untrans CompilationUnit where
    untrans (CompilationUnit x y z) = T $ J.CompilationUnit (t x) (t y) (t z)
-}
mkInstance :: Name -> Name -> Name -> Name -> Name -> Name -> CompTrans [Dec]
mkInstance classNm funNm wrap unwrap targNm typNm = do inf <- lift $ reify typNm
                                                       targTyp <- getFullyAppliedType typNm
                                                       let nmTyps = simplifyDataInf inf
                                                       clauses <- mapM (uncurry $ mkClause wrap unwrap) nmTyps
                                                       let conTyp = ConT (transName typNm)
                                                       annPropInf <- view annotationProp
                                                       let instTyp = case annPropInf of
                                                                       Nothing  -> conTyp
                                                                       Just api -> foldl AppT (ConT ''(:&:)) [conTyp, api ^. annTyp]
                                                       return [ famInst targTyp
                                                              , inst clauses instTyp
                                                              ]
  where
    famInst targTyp = TySynInstD targNm (TySynEqn [ConT $ nameLab typNm] targTyp)

    inst clauses instTyp =  InstanceD []
                                      (AppT (ConT classNm) instTyp)
                                      [FunD funNm clauses]

mapConditionallyReplacing :: [a] -> (a -> b) -> (a -> Bool) -> [b] -> [b]
mapConditionallyReplacing src f p reps = go src reps
  where
    go [] _                      = []
    go (x:xs) (y:ys) | p x       = y   : go xs ys
    go (x:xs) l      | not (p x) = f x : go xs l
    go (_:_ ) []                 = error "mapConditionallyReplacing: Insufficiently many replacements"

mkClause :: Name -> Name -> Name -> [Type] -> CompTrans Clause
mkClause wrap unwrap con tps = do isAnn <- getIsAnn
                                  nms <- mapM (const $ lift $ newName "x") tps
                                  nmAnn <- lift $ newName "a"
                                  tps' <- applyCurSubstitutions tps
                                  let nmTps = zip nms tps'
                                  Clause <$> (sequence [pat isAnn nmTps nmAnn]) <*> (body nmTps nmAnn) <*> pure []
  where
    pat :: (Type -> Bool) -> [(Name, Type)] -> Name -> CompTrans Pat
    pat isAnn nmTps nmAnn = do isProp <- isPropagatingAnns
                               if isProp then
                                 return $ ConP '(:&:) [nodeP, VarP nmAnn]
                                else
                                 return nodeP
      where
        nonAnnNms = map fst $ filter (not.isAnn.snd) nmTps
        nodeP = ConP (transName con) (map VarP nonAnnNms)

    body :: [(Name, Type)] -> Name -> CompTrans Body
    body nmTps nmAnn = do annPropInf <- view annotationProp
                          args <- case annPropInf of
                                    Nothing  -> return $ map atom nmTps
                                    Just api -> do isAnn <- getIsAnn
                                                   let unProp = api ^. unpropAnn
                                                   let annVars = filter (isAnn.snd) nmTps
                                                   let annExps = unProp (VarE nmAnn) (length annVars)
                                                   return $ mapConditionallyReplacing nmTps atom (isAnn.snd) annExps
                          return $ makeRhs args
      where
        makeRhs :: [Exp] -> Body
        makeRhs args = NormalB $ AppE (ConE wrap) $ foldl AppE (ConE con) args

    atom :: (Name, Type) -> Exp
    atom (x, t) | elem t baseTypes = VarE x
    atom (x, _)                    = AppE (VarE unwrap) (VarE x)
