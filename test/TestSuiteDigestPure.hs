module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.Digest.Adler32 as A
import qualified Data.Digest.Pure.Adler32 as AP
import Test.QuickCheck
import Text.Printf

instance Arbitrary B.ByteString where
  arbitrary = do
    n <- choose (1, 10000000)
    l <- vector n
    return $ B.pack l

main = mapM_ (\(s, a) -> printf "%-25s: " s >> a) tests

tests = [("aldler32CompareZlib",  quickCheck adler32CompareZlib)]

adler32CompareZlib :: B.ByteString -> Bool
adler32CompareZlib str =
  adler32Pure == adler32
  where
    adler32Pure = AP.adler32 str
    adler32     = A.adler32  str
