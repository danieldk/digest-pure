{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.Digest.Adler32 as A
import qualified Data.Digest.CRC32 as C
import qualified Data.Digest.Pure.Adler32 as AP
import qualified Data.Digest.Pure.CRC32 as CP
import System.Exit
import Test.QuickCheck
import Text.Printf

instance Arbitrary B.ByteString where
  arbitrary = do
    n <- choose (1, 10000000)
    l <- vector n
    return $ B.pack l

main = do
  results <- mapM (\(s, a) -> printf "%-25s: " s >> a) tests
  if all succesful results then
    exitSuccess
  else
    exitFailure

succesful :: Result -> Bool
succesful (Success _ _ _) = True
succesful _               = False

tests = [
  ("adler32CompareZlib", quickCheckResult adler32CompareZlib),
  ("crc32CompareZlib",   quickCheckResult crc32CompareZlib) ]

adler32CompareZlib :: B.ByteString -> Bool
adler32CompareZlib str =
  AP.adler32 str == A.adler32 str

crc32CompareZlib :: B.ByteString -> Bool
crc32CompareZlib str =
  CP.crc32 str == C.crc32 str
