{-# LANGUAGE BangPatterns #-}

module Data.Digest.Pure.Adler32 () where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as B
import Data.Word (Word8, Word32)

class Adler32 a where
  adler32       :: a -> Word32
  adler32Update :: Word32 -> a -> Word32

data AdlerState = AdlerState {-# UNPACK #-} !Word32  {-# UNPACK #-} !Word32

instance Adler32 B.ByteString where
  adler32       = adler32Update adler32Initial
  adler32Update = adler32Update'

adler32Initial :: Word32
adler32Initial = 1

adler32Update' :: Word32 -> B.ByteString -> Word32
adler32Update' adlerState =
  combineAdler . loop (breakAdler adlerState)
  where
    nmax = 5552
    loop adlerState' str
      | B.null xs = adlerState'
      | otherwise = loop (adler32Chunk adlerState' xs) ys
        where
          (xs, ys) = B.splitAt nmax str

adler32Chunk :: AdlerState -> B.ByteString -> AdlerState
adler32Chunk adler =
  modAdler . B.foldl' sumAdler adler

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

