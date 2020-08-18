{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

-- |

module Main where

import Control.Monad ( liftM )
import Control.Monad.Identity ( runIdentity )

import Language.JavaScript.Parser ( parseFile )
import Language.JavaScript.Pretty.Printer.Extended ( prettyPrint )

import Data.Comp.Multi ( project )
import Data.Comp.Multi.Strategic ( Rewrite, GRewrite, (<+), tryR, allbuR, promoteRF, addFail )

import Cubix.Language.JavaScript.Parametric.Full

parse :: FilePath -> IO (Maybe (JSTerm JSASTL))
parse path = liftM (Just . translate) $ parseFile path

prettyJS :: JSTerm JSASTL -> String
prettyJS = prettyPrint . untranslate


vandalizeIdent :: Rewrite JSTerm JSIdentL
vandalizeIdent (project -> Just (JSIdentName a n)) = return $ iJSIdentName a (n ++ "_bar")
vandalizeIdent x                                   = return x

vandalize' :: Rewrite JSTerm JSExpressionL
vandalize' (project -> (Just (JSIdentifier a n))) = return $ iJSIdentifier a (n ++ "_foo")
vandalize' x                                      = return x

vandalize :: GRewrite JSTerm
vandalize = allbuR $ tryR $ (promoteRF $ addFail vandalize') <+ (promoteRF $ addFail vandalizeIdent)

main = do
  Just tree <- parse "Foo.js"
  let tree' = runIdentity $ vandalize tree
  print tree'
  putStrLn $ prettyJS tree'
