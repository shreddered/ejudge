module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad (unless)

import Data.Array.Unboxed (UArray, (//), (!), array, indices)
import Data.Bits (Bits, shiftL)
import Data.Bool (bool)
import Data.Word

import System.IO (isEOF)

-- list of prime numbers
primes :: Integral a => [a]
primes = sieve [2..]

-- sieve of Eratosthenes
sieve :: Integral a => [a] -> [a]
sieve (x:xs) = let multof x n = n `rem` x == 0
                in x : (sieve (filter (not . multof x) xs))

-- 31st Mersenne number
mersenne31 :: (Num a, Bits a) => a
mersenne31 = (1 `shiftL` 31) - 1

mkHashes :: Word64 -> Word64 -> [Word64 -> Word64]
mkHashes m k = let hash i p = \x -> ((i * x + p) `mod` mersenne31) `mod` m
                in zipWith hash [1..k] (primes)

-- Bloom filter
data BloomFilter a = BloomFilter [a -> Word64] (UArray Word64 Bool)

instance Show (BloomFilter a) where
  show (BloomFilter _ arr) = [bool '0' '1' (arr ! i) | i <- indices arr]

-- create Bloom filter
bloomFilter :: Word64                       -- size of bit vector
            -> [a -> Word64]                -- hash functions
            -> BloomFilter a
bloomFilter m hashes = let arr = array (0, m - 1) [(i, False) | i <- [0..m - 1]]
                        in BloomFilter hashes arr

-- insert element in a Bloom filter
insert :: a -> BloomFilter a -> BloomFilter a
insert x (BloomFilter hashes arr) =
  let arr' = arr // [(i, True) | i <- (hashes <*> pure x)]
   in BloomFilter hashes arr'

-- search for an element in a Bloom filter
search :: a -> BloomFilter a -> Bool
search x (BloomFilter hashes arr) = all id [arr ! i | i <- (hashes <*> pure x)]

main :: IO ()
main = processLineByLine Nothing

-- Parsing routine
data Command = Set Word64 Double | Add Word64 | Search Word64 | Print | ErrorCommand

processLineByLine :: Maybe (BloomFilter Word64) -> IO ()
processLineByLine bf = do
  done <- isEOF
  unless done $ do
    line <- getLine
    let cmd = toCommand line
        (output, bf') = execute cmd bf
    unless (null output) (putStrLn output)
    processLineByLine bf'


toCommand :: String -> Command
toCommand str = let (cmd:args) = words str
                    n = read (args !! 0)
                    p = read (args !! 1)
                    k = read (args !! 0)
                 in case cmd of
                      "set"    -> Set n p
                      "add"    -> Add k
                      "search" -> Search k
                      "print"  -> Print
                      _        -> ErrorCommand

execute :: Command -> Maybe (BloomFilter Word64) -> (String, Maybe (BloomFilter Word64))
execute cmd Nothing   = case paramsFromSet cmd of
                          (Just (m, k)) -> ( (show m) ++ " " ++ (show k)
                                           , Just (bloomFilter m (mkHashes m k))
                                           )
                          Nothing -> ("error", Nothing)
execute cmd (Just bf) = case cmd of
                          (Set _ _)    -> ("error", Just bf)
                          ErrorCommand -> ("error", Just bf)
                          (Add k)      -> ("", Just (insert k bf))
                          (Search k)   -> ( bool "0" "1" (search k bf)
                                          , Just bf
                                          )
                          Print        -> (show bf, Just bf)

paramsFromSet :: Command -> Maybe (Word64, Word64)
paramsFromSet (Set n p) = let m = round (-fromIntegral(n) * (logBase 2 p) / log 2)
                              k = round (-(logBase 2 p))
                           in if p /= 0 && m /= 0 && k /= 0
                                 then Just (m, k)
                                 else Nothing
paramsFromSet _         = Nothing
