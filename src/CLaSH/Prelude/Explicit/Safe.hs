{-|
Copyright  :  (C) 2013-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

__This is the <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/safe-haskell.html Safe> API only of "CLaSH.Prelude.Explicit"__

This module defines the explicitly clocked counterparts of the functions
defined in "CLaSH.Prelude".

This module uses the explicitly clocked 'Signal'' synchronous signals, as
opposed to the implicitly clocked 'Signal' used in "CLaSH.Prelude". Take a
look at "CLaSH.Signal.Explicit" to see how you can make multi-clock designs
using explicitly clocked signals.
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeOperators    #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module CLaSH.Prelude.Explicit.Safe
  ( -- * Creating synchronous sequential circuits
    mealy#
  , mealyB#
  , moore#
  , mooreB#
  , registerB#
    -- * Synchronizer circuits for safe clock domain crossing
  , dualFlipFlopSynchronizer
  , asyncFIFOSynchronizer
    -- * ROMs
  , rom#
  , romPow2#
    -- * RAM primitives with a combinational read port
  , asyncRam#
  , asyncRamPow2#
    -- * BlockRAM primitives
  , blockRam#
  , blockRamPow2#
    -- ** BlockRAM read/write conflict resolution
  , readNew#
    -- * Utility functions
  , isRising#
  , isFalling#
    -- * Exported modules
    -- ** Explicitly clocked synchronous signals
  , module CLaSH.Signal.Explicit
  )
where

import Control.Applicative        (liftA2)
import GHC.Stack                  (HasCallStack)
import Prelude                    hiding (repeat)

import CLaSH.Prelude.BlockRam     (blockRam#, blockRamPow2#, readNew#)
import CLaSH.Prelude.Mealy        (mealy#, mealyB#)
import CLaSH.Prelude.Moore        (moore#, mooreB#)
import CLaSH.Prelude.RAM          (asyncRam#,asyncRamPow2#)
import CLaSH.Prelude.ROM          (rom#, romPow2#)
import CLaSH.Prelude.Synchronizer (dualFlipFlopSynchronizer,
                                   asyncFIFOSynchronizer)
import CLaSH.Signal               (Clock,Reset,Signal)
import CLaSH.Signal.Bundle        (Bundle(..))
import CLaSH.Signal.Explicit

{- $setup
>>> :set -XDataKinds -XMagicHash -XTypeApplications
>>> import CLaSH.Prelude
>>> import qualified Data.List as L
>>> type DomA = 'Domain "A" 100
>>> let clkA = Clock @DomA (signal True)
>>> let rstA = unsafeToAsyncReset# @DomA (fromList (False : L.repeat True))
>>> let rP = registerB# rstA clkA (8::Int,8::Int)
-}

{-# INLINE registerB# #-}
-- | Create a 'register' function for product-type like signals (e.g.
-- @('Signal' a, 'Signal' b)@)
--
-- @
-- type ClkA = 'Clk' \"A\" 100
--
-- clkA :: 'SClock' ClkA
-- clkA = 'sclock'
--
-- rP :: ('Signal'' ClkA Int, 'Signal'' ClkA Int) -> ('Signal'' ClkA Int, 'Signal'' ClkA Int)
-- rP = 'registerB#' clkA (8,8)
-- @
--
-- >>> simulateB# rP [(1,1),(2,2),(3,3)] :: [(Int,Int)]
-- [(8,8),(1,1),(2,2),(3,3)...
-- ...
registerB# :: (HasCallStack, Bundle a) => Reset res dom -> Clock clk dom -> a -> Unbundled dom a -> Unbundled dom a
registerB# res clk i = unbundle Prelude.. register# res clk i Prelude.. bundle

{-# INLINABLE isRising# #-}
-- | Give a pulse when the 'Signal'' goes from 'minBound' to 'maxBound'
isRising# :: (HasCallStack, Bounded a, Eq a)
          => Reset res dom
          -> Clock clk dom
          -> a -- ^ Starting value
          -> Signal dom a
          -> Signal dom Bool
isRising# res clk is s = liftA2 edgeDetect prev s
  where
    prev = register# res clk is s
    edgeDetect old new = old == minBound && new == maxBound

{-# INLINABLE isFalling# #-}
-- | Give a pulse when the 'Signal'' goes from 'maxBound' to 'minBound'
isFalling# :: (HasCallStack, Bounded a, Eq a)
           => Reset res dom
           -> Clock clk dom
           -> a -- ^ Starting value
           -> Signal dom a
           -> Signal dom Bool
isFalling# res clk is s = liftA2 edgeDetect prev s
  where
    prev = register# res clk is s
    edgeDetect old new = old == maxBound && new == minBound
