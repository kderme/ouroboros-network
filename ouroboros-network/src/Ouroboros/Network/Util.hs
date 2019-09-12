{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}   -- for Rep ForgettingFields

module Ouroboros.Network.Util (
    gshowsPrecForgetFields,
    GShowK1 (..),
  ) where

import           Data.Coerce
import           GHC.Generics

import qualified Generics.Deriving.Show as GD

-- | Implements 'showsPrec' as if the constructors' record fields were instead
-- plain arguments
--
gshowsPrecForgetFields ::
  forall a.
     ( Generic a
     , Coercible (Rep a) (ForgetFields (Rep a))
     , GD.GShow' (ForgetFields (Rep a))
     )
  => Int -> a -> ShowS
gshowsPrecForgetFields p =
    GD.gshowsPrecdefault p . ForgettingFields

{-------------------------------------------------------------------------------
  Forgetting fields
-------------------------------------------------------------------------------}

newtype ForgettingFields a = ForgettingFields a

-- Without this definition, GHC cannot make the same inference at the usage
-- sites within this module, probably because f and g involve type families
--
coerce1 :: Coercible f g => f a -> g a
coerce1 x = coerce x

instance
  ( Generic a
  , Coercible (Rep a) (ForgetFields (Rep a))
  ) => Generic (ForgettingFields a)
  where
    type Rep (ForgettingFields a) = ForgetFields (Rep a)

    to = ForgettingFields . to . coerce1
    from (ForgettingFields a) = (coerce1 . from) a

type family ForgetFields (rep :: * -> *) :: * -> * where
    -- the interesting case
    ForgetFields (M1 i c f) = M1 i (ForgetFieldsMeta c) (ForgetFields f)

    -- the tedious case
    --
    -- GShow' (K1 i c) requires GShow c, whereas we want it to use Show c
    -- instead
    ForgetFields (K1 i c)     = GShowK1 i c

    -- the mundane recursive cases
    ForgetFields (a :+: b)  = ForgetFields a :+: ForgetFields b
    ForgetFields (a :*: b)  = ForgetFields a :*: ForgetFields b
    ForgetFields (a :.: b)  = ForgetFields a :.: ForgetFields b

    -- the mundane base cases
    ForgetFields V1           = V1
    ForgetFields U1           = U1
    ForgetFields UChar        = UChar
    ForgetFields UDouble      = UDouble
    ForgetFields UFloat       = UFloat
    ForgetFields UInt         = UInt
    ForgetFields UWord        = UWord

-- | Remove data about field selectors
type family ForgetFieldsMeta (rep :: Meta) :: Meta where
    ForgetFieldsMeta ('MetaSel 'Nothing  su ss ds) = 'MetaSel 'Nothing su ss ds
    ForgetFieldsMeta ('MetaSel ('Just _) su ss ds) = 'MetaSel 'Nothing su ss ds
    ForgetFieldsMeta ('MetaCons n f s)             = 'MetaCons n f 'False
    ForgetFieldsMeta ('MetaData n m p nt)          = 'MetaData n m p nt

-- | Merely an implementation detail
--
-- This constructor must be in scope at usage sites of 'gshowsPrecForgetFields'
-- for the 'Coercible' constraint to be satisfiable.
--
newtype GShowK1 i c p = GShowK1 (K1 i c p)

instance Show c => GD.GShow' (GShowK1 i c) where
    gshowsPrec' _ p (GShowK1 (K1 c)) = showsPrec p c
