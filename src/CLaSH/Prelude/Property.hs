{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Unsafe #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Prelude.Property
  ( -- * Verification of circuit definitions
    verifyProperty -- :: Bit -> String -> a -> a
  )
where

import Debug.Trace           (trace)
import GHC.TypeLits          (KnownNat)
import Prelude               hiding ((!!), length)

import CLaSH.Sized.BitVector (Bit, low, high)

{-# NOINLINE verifyProperty #-}
-- | Lorem ipsum...
verifyProperty :: Bool -> String -> a -> a
verifyProperty True  _ a = a
verifyProperty False s a =
  trace (concat [ "property violated: '"
                , s
                , "', got `low` when `high` was expected!"
                ]) a
