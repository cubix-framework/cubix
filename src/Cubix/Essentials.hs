{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Cubix.Essentials
-- Copyright   :  (c) 2020 James Koppel
-- License     :  BSD3
--
-- This is a beginner-friendly package containing the most common functions and
-- types needed by Cubix programs. It is built as a companion to the Cubix tutorial,
-- though we explain many things in both places.
--
-- __Guide to using this file__
-- * __Strongly recommend__: Click "Collapse All Instances" in the top-right corner
-- * For newcomers: Look at the examples given, not at the types
-- * This file is not the authoritative source for any of its exports. Some
--   typeclasses have methods hidden here. Some functions are exported
--   with more-specific type signatures that are easier to understand. An
--   unfortunate consequence of this is that __some definitions in this file
--   conflict with the original Cubix definitions__.
--
--
--------------------------------------------------------------------------------


module Cubix.Essentials (

    -- * Core representation
    --
    -- ** Language fragments
    -- $fragments
    --   TODO (**TODO**): Discuss smart constructors somewhere

    deriveAll

    -- ** Signatures: Combining fragments
    -- $sums

  , O.Sum

    -- ** Terms

    -- | Terms are how we put stuff together
  , Term
  , inject

    -- ** Dealing with sums
  , (O.:-<:)
  , O.All
  , caseCxt

    -- ** Higher topics involving sums/tems

    -- *** Sort injections
  , InjF(injF, projF)

    -- *** Labeled nodes
  , Label
  , TermLab

    -- *** Common typeclassess
    --
    -- | These are analogues of standard typeclasses, but for sorted unfixed datatypes.
    --   You are unlikely to use methods in these classes directly, but
    --   you may frequently invoke functions that require them. You may
    --   wind up writing many constraints that use these, likely combined with the `All`
    --   constructor.
    --
  , O.HFunctor
  , O.HFoldable
  , O.HTraversable
  , O.EqHF
  , O.OrdHF
  , O.ShowHF

    -- * Manipulating terms

  , project

    -- ** Dealing with annotations/labeled nodes
    -- *** Adding labels
  , MonadAnnotater
  , annotateLabel
  , runConcurrentSupplyLabeler

    -- *** Removing labels
  , project'
  , stripA

    -- ** Dealing with containers
  , InsertF(..)
  , ExtractF(..)

    -- ** Traversals

    -- *** Without strategy combinators
    -- | Cubix inherits from @compdata@ two elementary traversal functions.
  , query
  , transform

    -- *** With strategy combinators
    --
    -- | The standard @`(>=>)`@ operator is also
    --   useful for sequentially combining `RewriteM`'s.

  , RewriteM
  , (>+>)
  , alltdR
  , prunetdR

    -- **** Things used for generalizing sort-specific functions
  , DynCase
  , addFail
  , promoteRF
  , promoteR

    -- * Languages

    -- ** Signatures

    -- |
    --   The primary language signatures in Cubix are `MCSig`, `MJavaSig`, `MJSSig`, `MLuaSig`, and `MPythonSig`.
    --   The "M" stands for "modular."
    --
    --   These are all long, auto-generated definitions consisting of every language
    --   fragment in the language (often over 80). We re-export them here, but with their definitions hidden.

  , MCSig
  , MJavaSig
  , MJSSig
  , MLuaSig
  , MPythonSig
    -- ** Terms
  , MCTerm
  , MJavaTerm
  , MJSTerm
  , MLuaTerm
  , MPythonTerm

    -- ** Labeled terms
  , MCTermLab
  , MJavaTermLab
  , MJSTermLab
  , MLuaTermLab
  , MPythonTermLab

    -- ** Parsing / pretty-printing
  , ParseFile(..)
  , Pretty(pretty)


    -- * Control-flow graphs
    -- ** Representation
  , Cfg
  , CfgNode
  , safeLookupCfg
  , lookupCfg
  , cfgNodeForTerm

    -- ** Construction
  , CfgBuilder
  , makeCfg

    -- ** CFG-based inserter
    -- *** Other relevant data types and operations
  , ProgInfo
  , makeProgInfo
  , containingCfgNode
  , withContainingCfgNode

    -- ** CFG-based inserter proper
  , InsertAt
  , MonadCfgInsertion(dominatingPrependFirst)
  , CfgInserterT
  , performCfgInsertions

  ) where


---------------------------------------------------
----- For re-exports
---------------------------------------------------

import qualified Data.Comp.Multi as O
import Data.Comp.Multi.Strategic
import Data.Comp.Multi.Strategy.Classification

import Cubix.Sin.Compdata.Annotation

import Cubix.Language.Info
import Cubix.ParsePretty

import qualified Cubix.Language.C.Parametric.Common as C
import Cubix.Language.C.Parametric.Common hiding ( MCSig )
import qualified Cubix.Language.Java.Parametric.Common as Java
import Cubix.Language.Java.Parametric.Common hiding ( MJavaSig )
import qualified Cubix.Language.JavaScript.Parametric.Common as JS
import Cubix.Language.JavaScript.Parametric.Common hiding ( MJSSig )
import qualified Cubix.Language.Lua.Parametric.Common as Lua
import Cubix.Language.Lua.Parametric.Common hiding ( MLuaSig )
import qualified Cubix.Language.Python.Parametric.Common as Python
import Cubix.Language.Python.Parametric.Common hiding ( MPythonSig )

import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.ProgInfo
import Cubix.Language.Parametric.Semantics.Cfg
import Cubix.Language.Parametric.Semantics.CfgInserter
import Cubix.Language.Parametric.Semantics.SemanticProperties

---------------------------------------------------
----- For references
---------------------------------------------------

import Control.Monad ( (>=>) )

---------------------------------------------------
----- For re-exports
---------------------------------------------------

import Data.Proxy ( Proxy )

---------------------------------------------------
----- Longer docs
---------------------------------------------------


{- $fragments
  The first concept is that of a language fragment and signature. Language fragments in Cubix look like this:

  @
  data ExpL
  data StatementL

  data Statement e l where
    Assign :: String -> e ExpL             -> Statement e StatementL
    Seq    :: e StatementL -> e StatementL -> Statement e StatementL
  @

   This definition of @Statement@ is called a *language fragment*. It defines
   the nodes @Assign@ and @Seq@ without any reference to the larger language
   they are embedded in. The *labels* @StatementL@ and @ExpL@ are *sorts*, categories of terms.
   Popular languages commonly have a sort for top-level definitions, a sort for lvalues, a sort for
   import statements, etc. Specifying the sort separately from the language fragment makes it possible
   to modularly add new statements and expressions to a language definition, which we will demonstrate shortly.

  Expect to see the kind @(* -> *) -> * -> *@ a lot, the kind of language fragments.

  When you define a normal datatype (kind @*@) in Haskell, you might suffix it with
  @deriving (Eq, Ord, Show)@. The equivalent for these higher-kinded language fragments is
  to derive their higher-kinded equivalents using the `deriveAll` Template Haskell function, e.g.:

  @
  deriveAll [''Statement]
  @
-}


{- $sums
 Language fragments let one define nodes independent of a larger language. We shall soon
 show how to combine them into a **signature**. But first, let us define two more
 language fragments, @Conditional@ and @Exp@.

 @
 data Conditional e l where
    If :: e ExpL -> e StatementL -> e StatementL -> Conditional e StatementL

 data Exp e l where
    VarExp :: String           -> Exp e ExpL
    Add    :: e ExpL -> e ExpL -> Exp e ExpL
    -- other cases omitted
 @

 As a preview, note how @Conditional@ defines a new node @If@ of sort @Statement@. When these fragments are
 combined into a language, the @If@ node will be able to be included under a @Seq@ node, just as easily
 as if was defined in the @Statement@ fragment.

 A *signature* is a collection of language fragments. They take the form of a type-level list.
 Here is a signature for a language containing all the nodes in the @Statement@, @Conditional@, and @Exp@
 fragments.

 @
 type LangSig = '[Statement, Conditional, Exp]
 @

 Signatures have the kind @[(* -> *) -> * -> *]@, another kind you'll encounter frequently.
 Signatures are used by passing them to the `Sum` constructor, e.g.:

 @
 Sum LangSig
 @

 @Sum LangSig@ has kind @(* -> *) -> * -> *@, the same kind as language fragments. It is
 conceptually a single language fragment consisting of the combined nodes of @Statement@, @Conditional@,
 and @Exp@. It could even be placed in another language signature, and nested within another `Sum`,
 although this use case is not supported.

 The `Sum` constructor is important to Cubix, but you may not use it directly.
 More often, you'll use something like @Term LangSig@ --- but note that `Term` is defined using
 `Sum`.
-}


------------------------------------------------------------------------------------------
---- Haddock has a limitation where hideous kind signatures appear
---- in re-exported functions (at least from a different package). We thus redefine a lot
---- of things here so that the docs look prettier. Does not work for typeclasses, alas.
---- Has the side-effect of nuking the original docstring.
----
---- While we're at it, we can also simplify some type signatures.
------------------------------------------------------------------------------------------


---------------- Redefinitions for compdata ----------------------------------------------

type Term fs = O.HFix (O.Sum fs)

-- | For full documentaton, see original declaration: `Data.Comp.Multi.Annotation.project`
project' :: (O.RemA f f', s O.:<: f') => O.HFix f i -> Maybe (s (O.HFix f) i)
project' = O.project'

-- | For full documentaton, see original declaration: `Data.Comp.Multi.Annotation.stripA`
stripA :: (O.RemA f f', O.HFunctor f) => O.HFix f i -> O.HFix f' i
stripA = O.stripA

-- | For full documentaton, see original declaration: `Data.Comp.Multi.Sum.project`
--
-- Simpler definition:
--
-- @
-- project :: (g O.:-<: fs) => Term fs l -> Maybe (g (Term fs) l)
-- @
project :: (g O.:<: f) => O.Cxt h f a l -> Maybe (g (O.Cxt h f a) l)
project = O.project

-- | For full documentaton, see original declaration: `Data.Comp.Multi.Ops.caseCxt`
caseCxt :: (O.All cxt fs) => Proxy cxt -> (forall f. (cxt f) => f a e -> b) -> O.Sum fs a e -> b
caseCxt = O.caseCxt

inject :: (g O.:-<: fs) => g (Term fs) l -> Term fs l
inject = O.inject

-- | For full documentation, see original declaration: `Data.Comp.Multi.Generic.transform`
transform :: (O.All O.HFunctor fs) => (forall i. Term fs i -> Term fs i) -> Term fs l -> Term fs l
transform = O.transform

-- | For full documentation, see original declaration: `Data.Comp.Multi.Generic.query` `O.query`
query :: (O.All O.HFoldable fs, O.All O.HFunctor fs) => (forall i. Term fs i -> r) -> (r -> r -> r) -> Term fs l -> r
query = O.query



---------------- Redefinitions for Cubix --------------------------------------------------


type MCSig = C.MCSig
type MJavaSig = Java.MJavaSig
type MJSSig = JS.MJSSig
type MLuaSig = Lua.MLuaSig
type MPythonSig = Python.MPythonSig