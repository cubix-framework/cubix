-- |

module Main where

import Control.Monad.Identity ( runIdentity )
import Data.Proxy ( Proxy(..) )

import Language.Python.Common ( pretty )
import Language.Python.Version3.Parser ( parseModule )

import Data.Comp.Multi ( Sum, project, (:&:)(..), HFix, unTerm, Cxt(..), HFunctor(..), AnnTerm )
import Data.Comp.Multi.Strategic ( Rewrite, GRewrite, allbuR, promoteR, addFail )

import Cubix.Language.Python.Parametric.Full

import Cubix.Language.Parametric.Syntax hiding ( Ident, IdentL, iIdent )
import Data.Comp.Multi.Strategy.Classification
import Cubix.Language.Parametric.Semantics.SemanticProperties


parse :: FilePath -> IO (Maybe (PythonTerm ModuleL))
parse path = do
  contents <- readFile path
  let res = parseModule contents path
  case res of
    Left  e     -> print e >> return Nothing
    Right (m, _) -> return $ Just $ translate $ fmap (const ()) m

prettyPython :: PythonTerm ModuleL -> String
prettyPython = show . pretty . untranslate


vandalize' :: Rewrite PythonTerm IdentL
vandalize' (project -> (Just (Ident t a))) = return $ iIdent (t ++ "_foo") a

vandalize :: GRewrite PythonTerm
vandalize = allbuR $ promoteR $ addFail vandalize'

annUnit :: (HFunctor f) => HFix f l -> HFix (f :&: ()) l
annUnit (Term x) = Term ((hfmap annUnit x) :&: ())

main = do
  Just tree <- parse "Foo.py"
  let tree' = runIdentity $ vandalize tree
  print tree'
  putStrLn $ prettyPython tree'

  let (lst :: PythonTerm [StatementL]) = ConsF' (iPass iUnitF) NilF'
  print $ isSort @[StatementL] (annUnit lst)
  print $ canInsertBefore (Proxy @StatementL) (annUnit lst)

  let (lstf :: (Sum PythonSig :&: ()) (AnnTerm () PythonSig) [StatementL]) = unTerm (annUnit lst)
  print $ kIsSort @[StatementL] lstf
