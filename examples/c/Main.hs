{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | 

module Main where

import Control.Monad.Identity ( runIdentity )

import Language.C ( readInputStream, parseC, pretty )
import qualified Language.C.Syntax as Orig
import qualified Language.C.Data.Node as Node
import Language.C.Data.Position ( initPos, nopos )


import Data.Comp.Multi
import Data.Comp.Multi.Strategic ( Rewrite, GRewrite, allbuR, promoteR, addFail )

import qualified Cubix.Language.C.Parametric.Common as Common
import Cubix.Language.C.Parametric.Full

parse :: FilePath -> IO (Maybe (CTerm CTranslationUnitL))
parse path = do
  contents <- readInputStream path
  case parseC contents (initPos path) of
    Left errors -> print errors >> return Nothing
    Right tree -> return $ Just $ translate $ fmap (const ()) tree


dummyNodeInfo :: Node.NodeInfo
dummyNodeInfo = Node.mkNodeInfoOnlyPos nopos

prettyC :: CTerm CTranslationUnitL -> String
prettyC tree = show $ pretty $ fmap (const dummyNodeInfo) $ untranslate tree


pattern PIdent s x y <- (project -> (Just (Ident s x y)))

vandalize' :: Rewrite CTerm IdentL
vandalize' (PIdent s x y) = return $ iIdent (s ++ "_foo") x y

vandalize :: GRewrite CTerm
vandalize = allbuR $ promoteR $ addFail vandalize'

main = do
  Just tree <- parse "Foo.c"
  let tree' = Common.translate tree
  print tree'
  putStrLn $ prettyC $ runIdentity $ vandalize tree
