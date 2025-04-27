module Data.Comp.Trans.DeriveMulti (
    deriveMulti
  ) where

import Control.Lens ( _1, _2, _3, (&), (%~), (%%~), (^.), view )
import Control.Monad ( liftM )
import Control.Monad.Trans ( MonadTrans(lift) )


import Language.Haskell.TH.Syntax hiding ( lift )
import Language.Haskell.TH.ExpandSyns ( expandSyns )

import Data.Comp.Trans.Util

deriveMulti :: Name -> CompTrans [Dec]
deriveMulti n = do
  inf <- CompTrans $ lift $ reify n
  substs <- view substitutions
  typeArgs <- getTypeArgs n

  if containsAll substs typeArgs then
    case inf of
      TyConI (DataD _ nm _ _ cons _)   -> mkGADT nm (applySubsts substs cons)
      TyConI (NewtypeD _ nm _ _ con _) -> mkGADT nm [(applySubsts substs con)]
      _                              -> do CompTrans $ lift $ reportError $ "Attempted to derive multi-sorted compositional data type for " ++ show n
                                                              ++ ", which is not a nullary datatype (and does not have concrete values supplied for type args)"
                                           return []
   else
    do CompTrans $ lift $ reportError $ "Attempted to derive multi-sorted compositional data type for " ++ show n
                            ++ " but it has type arguments which are not substituted away"
       return []

checkUniqueVar :: Con -> CompTrans ()
checkUniqueVar con = if length (filter isVar fields) <= 1 then
                       return ()
                     else
                       fail $ "comptrans: Multiple annotion fields in constructor:" ++ show con
  where
    fields :: [Type]
    fields = case con of
      RecC _ sts    -> map (^. _3) sts
      NormalC _ sts -> map (^. _2) sts
      _             -> error $ "Attempted to derive multi-sorted compositional datatype for something with non-normal constructors: " ++ show con

mkGADT :: Name -> [Con] -> CompTrans [Dec]
mkGADT n cons = do
  e <- CompTrans $ lift $ newName "e"
  i <- CompTrans $ lift $ newName "i"
  let n' = transName n
  annProp <- view annotationProp
  case annProp of
    Just _annPropInf  -> mapM_ checkUniqueVar cons
    Nothing          -> return ()
  cons' <- mapM (mkCon n' e i) cons
  return $ [DataD [] n' [KindedTV e BndrReq (AppT (AppT ArrowT StarT) StarT), PlainTV i BndrReq] Nothing cons' []
           ,DataD [] (nameLab n) [] Nothing [] []
           ]

mkCon :: Name -> Name -> Name -> Con -> CompTrans Con
mkCon l e i (NormalC n sts) = view annotationProp >>= mkConNormal
   where
     mkConNormal annPropInfo = ForallC [] ctx <$> inner
      where
        ctx = [foldl AppT EqualityT [(VarT i), (ConT $ nameLab l)]]

        sts'  = case annPropInfo of
                  Just api -> filter (not . (api ^. isAnnotation) . (^. _2)) sts
                  Nothing  -> sts
        sts'' = sts' & (traverse._2) %%~ unfixType e
        inner = liftM (NormalC (transName n)) sts''
mkCon l e i (RecC n vsts) = view annotationProp >>= mkConRec
  where
    mkConRec annPropInfo = ForallC [] ctx <$> inner
      where
        ctx = [foldl AppT EqualityT [(VarT i), (ConT $ nameLab l)]]

        vsts'   = case annPropInfo of
                   Just api -> filter (not . (api ^. isAnnotation) . (^. _3)) vsts
                   Nothing  -> vsts
        vsts''  = vsts'  & (traverse._1) %~ transName
        vsts''' = vsts'' & (traverse._3) %%~ unfixType e
        inner   = liftM (RecC (transName n)) vsts'''
mkCon _ _ _ c = fail $ "Attempted to derive multi-sorted compositional datatype for something with non-normal constructors: " ++ show c

unfixType :: Name -> Type -> CompTrans Type
unfixType _ t | elem t baseTypes = return t
unfixType e t = do checkAnn <- getIsAnn
                   t' <- (CompTrans $ lift (expandSyns t)) >>= getLab checkAnn
                   return $ AppT (VarT e) t'


