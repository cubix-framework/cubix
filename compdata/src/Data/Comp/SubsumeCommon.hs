{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds            #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.SubsumeCommon
-- Copyright   :  (c) 2014 Patrick Bahr
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- Shared parts of the implementation of signature subsumption for
-- both the base and the multi library.
--
--------------------------------------------------------------------------------

module Data.Comp.SubsumeCommon
    ( ComprEmb
    , Pos (..)
    , Emb (..)
    , Choose
    , Sum'
    , Proxy (..)
    ) where

-- | This type is used in its promoted form only. It represents
-- pointers from the left-hand side of a subsumption to the right-hand
-- side.
data Pos = Here | Le Pos | Ri Pos | Sum Pos Pos

-- | This type is used in its promoted form only. It represents
-- possible results for checking for subsumptions. 'Found' indicates a
-- subsumption was found; 'NotFound' indicates no such subsumption was
-- found. 'Ambiguous' indicates that there are duplicates on the left-
-- or the right-hand side.
data Emb = Found Pos | NotFound | Ambiguous

data Proxy a = P


type family Choose (e1 :: Emb) (r :: Emb) :: Emb where
    Choose (Found x) (Found y) = Ambiguous
    Choose Ambiguous y = Ambiguous
    Choose x Ambiguous = Ambiguous
    Choose (Found x) y = Found (Le x)
    Choose x (Found y) = Found (Ri y)
    Choose x y = NotFound


type family Sum' (e1 :: Emb) (r :: Emb) :: Emb where
    Sum' (Found x) (Found y) = Found (Sum x y)
    Sum' Ambiguous y = Ambiguous
    Sum' x Ambiguous = Ambiguous
    Sum' NotFound y = NotFound
    Sum' x NotFound = NotFound


-- | This type family takes a position type and compresses it. That
-- means it replaces each nested occurrence of
--
-- @
--   Sum (prefix (Le Here)) (prefix (Ri Here))@
-- @
---
-- with
--
-- @
--   prefix Here@
-- @
--
-- where @prefix@ is some composition of @Le@ and @Ri@. The rational
-- behind this type family is that it provides a more compact proof
-- term of a subsumption, and thus yields more efficient
-- implementations of 'inj' and 'prj'.

type family ComprPos (p :: Pos) :: Pos where
    ComprPos Here = Here
    ComprPos (Le p) = Le (ComprPos p)
    ComprPos (Ri p) = Ri (ComprPos p)
    ComprPos (Sum l r) = CombineRec (ComprPos l) (ComprPos r)


-- | Helper type family for 'ComprPos'. Note that we could have
-- defined this as a type synonym. But if we do that, performance
-- becomes abysmal. I presume that the reason for this huge impact on
-- performance lies in the fact that right-hand side of the defining
-- equation duplicates the two arguments @l@ and @r@.
type family CombineRec l r where
    CombineRec l r = CombineMaybe (Sum l r) (Combine l r)

-- | Helper type family for 'ComprPos'.
type family CombineMaybe (p :: Pos) (p' :: Maybe Pos) where
    CombineMaybe p (Just p') = p'
    CombineMaybe p p'        = p


-- | Helper type family for 'ComprPos'.
type family Combine (l :: Pos) (r :: Pos) :: Maybe Pos where
    Combine (Le l) (Le r) = Le' (Combine l r)
    Combine (Ri l) (Ri r) = Ri' (Combine l r)
    Combine (Le Here) (Ri Here) = Just Here
    Combine l r = Nothing

-- | 'Ri' lifted to 'Maybe'.
type family Ri' (p :: Maybe Pos) :: Maybe Pos where
    Ri' Nothing = Nothing
    Ri' (Just p) = Just (Ri p)

-- | 'Le' lifted to 'Maybe'.
type family Le' (p :: Maybe Pos) :: Maybe Pos where
    Le' Nothing = Nothing
    Le' (Just p) = Just (Le p)


-- | If the argument is not 'Found', this type family is the
-- identity. Otherwise, the argument is of the form @Found p@, and
-- this type family does two things: (1) it checks whether @p@ the
-- contains duplicates; and (2) it compresses @p@ using 'ComprPos'. If
-- (1) finds no duplicates, @Found (ComprPos p)@ is returned;
-- otherwise @Ambiguous@ is returned.
--
-- For (1) it is assumed that @p@ does not contain 'Sum' nested
-- underneath a 'Le' or 'Ri' (i.e. only at the root or underneath a
-- 'Sum'). We will refer to such positions below as /atomic position/.
-- Positions not containing 'Sum' are called /simple positions/.
type family ComprEmb (e :: Emb) :: Emb where
    ComprEmb (Found p) = Check (Dupl p) (ComprPos p)
    ComprEmb e = e

-- | Helper type family for 'ComprEmb'.
type family Check (b :: Bool) (p :: Pos) where
    Check False p = Found p
    Check True  p = Ambiguous

-- | This type family turns a list of /atomic position/ into a list of
-- /simple positions/ by recursively splitting each position of the
-- form @Sum p1 p2@ into @p1@ and @p2@.
type family ToList (s :: [Pos]) :: [Pos] where
    ToList (Sum p1 p2 ': s) = ToList (p1 ': p2 ': s)
    ToList (p ': s) = p ': ToList s
    ToList '[] = '[]

-- | This type checks whether the argument (atomic) position has
-- duplicates.
type Dupl s = Dupl' (ToList '[s])

-- | This type family checks whether the list of positions given as an
-- argument contains any duplicates.
type family Dupl' (s :: [Pos]) :: Bool where
    Dupl' (p ': r) = OrDupl' (Find p r) r
    Dupl' '[] = False

-- | This type family checks whether its first argument is contained
-- its second argument.
type family Find (p :: Pos) (s :: [Pos]) :: Bool where
    Find p (p ': r)  = True
    Find p (p' ': r) = Find p r
    Find p '[] = False

-- | This type family returns @True@ if the first argument is true;
-- otherwise it checks the second argument for duplicates.
type family OrDupl' (a :: Bool) (b :: [Pos]) :: Bool where
    OrDupl'  True  c  = True
    OrDupl'  False c  = Dupl' c
