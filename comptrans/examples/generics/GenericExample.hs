module GenericExample where

import Control.DeepSeq ( NFData(rnf) )
import Control.DeepSeq.Generics

import qualified Data.Comp.Multi as M
import Data.Comp.Multi.Derive ( derive, makeHFunctor, makeHTraversable, makeHFoldable, makeEqHF, makeShowHF, makeOrdHF, smartConstructors )
import qualified Data.Comp.Multi.Ops as M

import GHC.Generics ( Generic(..), (:*:)(..), (:+:)(..), K1(..), V1, Rec0, U1(..) )

import Language.Haskell.TH hiding ( Lit )

import Data.Comp.Derive.Generic

data ArithL
data AtomL
data LitL

data Arith e l where
   Add :: e (Maybe AtomL) -> e AtomL -> Arith e ArithL

data Atom e l where
   Var :: String -> Atom e AtomL
   Const :: e LitL -> Atom e AtomL

data Lit (e :: M.Family) l where
   Lit :: Int -> Lit e LitL

data MaybeF e l where
  NothingF :: MaybeF e (Maybe l)
  JustF :: e l -> MaybeF e (Maybe l)

derive [makeHFunctor, makeHTraversable, makeHFoldable, makeEqHF, makeShowHF,
                    makeOrdHF, smartConstructors]
       [''Arith, ''Atom, ''Lit]

type ArithSig = Arith M.:+: Atom M.:+: Lit M.:+: MaybeF

type ArithTerm = M.Term ArithSig

--from (iAdd (iConst $ iLit 1) (iConst $ iLit 2))


instance Generic (MaybeF e (Maybe l)) where
  type Rep (MaybeF e (Maybe l)) = U1 :+: Rec0 (e l)
  from NothingF = L1 $ U1
  from (JustF x) = R1 $ K1 x
  to (L1 U1) = NothingF
  to (R1 (K1 x)) = JustF x



makeGeneric [''Arith, ''Atom, ''Lit] [ConT ''ArithL, ConT ''AtomL, ConT ''LitL, AppT (ConT ''Maybe) (VarT (mkName "l"))] 
makeGeneric [''MaybeF] [ConT ''ArithL, ConT ''AtomL, ConT ''LitL]

makeInstancesLike [''ArithTerm] [ConT ''ArithL, ConT ''AtomL, ConT ''LitL] [d| instance NFData GenericExample where rnf = genericRnfV1 |]

instance (NFData (ArithTerm l)) => NFData (ArithTerm (Maybe l)) where rnf = genericRnfV1

