{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Data.Digest.Pure.Adler32
-- Copyright   : (c) 2011 Daniël de Kok
-- License     : BSD3
--
-- Maintainer  : Daniël de Kok <me@danieldk.eu>
-- Stability   : experimental
--
-- This module provides functions to calculate Adler32 checksums.

module Data.Digest.Pure.Adler32 (
  Adler32(..)
) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8, Word32)

-- |
-- The class of values for which Adler32 checksums can be computed.
class Adler32 a where
  -- |
  -- Compute the Adler32 checksum.
  adler32       :: a -> Word32
  -- |
  -- Incrementally update an Adler32 checksum. The Adler32 checksum of one
  -- /A/ updated with /B/, equals the checksum of the concatenation of /A/ and
  -- /B/.
  adler32Update :: Word32 -> a -> Word32

data AdlerState = AdlerState {-# UNPACK #-} !Word32  {-# UNPACK #-} !Word32

instance Adler32 BL.ByteString where
  adler32       = adler32Update adler32Initial
  adler32Update = adler32Update'

instance Adler32 BS.ByteString where
  adler32             = adler32Update adler32Initial
  adler32Update state = adler32Update state . BL.fromChunks . return

adler32Initial :: Word32
adler32Initial = 1

adler32Update' :: Word32 -> BL.ByteString -> Word32
adler32Update' adlerState =
  combineAdler . loop (breakAdler adlerState)
  where
    nmax = 5552
    loop adlerState' str
      | BL.null xs = adlerState'
      | otherwise = loop (adler32Chunk adlerState' xs) ys
        where
          (xs, ys) = BL.splitAt nmax str

adler32Chunk :: AdlerState -> BL.ByteString -> AdlerState
adler32Chunk adler =
  modAdler . BL.foldl' sumAdler adler

breakAdler :: Word32 -> AdlerState
breakAdler adler =
  AdlerState (adler .&. 0xffff) ((adler `shiftR` 16) .&. 0xffff)

combineAdler :: AdlerState -> Word32
combineAdler (AdlerState a b) =
  (b `shiftL` 16) .|. a

sumAdler :: AdlerState -> Word8 -> AdlerState
sumAdler (AdlerState a b) !byte = AdlerState newA (sumB b newA)
  where
    newA = sumA a byte

sumA :: Word32 -> Word8 -> Word32
{-# INLINE sumA #-}
sumA !adlerA !byte = (adlerA + (fromIntegral byte))

sumB :: Word32 -> Word32 -> Word32
{-# INLINE sumB #-}
sumB !adlerB !adlerA = adlerB + adlerA

modAdler :: AdlerState -> AdlerState
modAdler (AdlerState adlerA adlerB) =
  AdlerState (adlerA `mod` 65521) (adlerB `mod` 65521)

