module Main where

import Control.Applicative

import Data.Array.Unboxed (UArray, (//), (!), array, indices)
import Data.Bits (Bits, shiftL)
import Data.Bool (bool)
import Data.Word

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

-- Bloom filter
data BloomFilter a = BloomFilter [a -> Int] (UArray Int Bool)

instance Show (BloomFilter a) where
  show (BloomFilter _ arr) = [bool '0' '1' (arr ! i) | i <- indices arr]

-- create Bloom filter
bloomFilter :: Int                       -- size of bit vector
            -> [a -> Int]                -- hash functions
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

-- Parsing routine
-- type for representing arbitrary command
data Command = Set Int Double | Add Word64 | Search Word64 | Print | ErrorCommand

main :: IO ()
main = interact (unlines . execute . map toCommand . filter (not . null) . lines)

-- parse command from string
toCommand :: String -> Command
toCommand s = let (command:args) = words s
                  n = read (args !! 0)
                  p = read (args !! 1)
                  k = read (args !! 0)
                  in case command of
                       "set"    -> Set n p
                       "add"    -> Add k
                       "search" -> Search k
                       "print"  -> Print
                       _        -> ErrorCommand

-- TODO: implement logic
execute :: [Command] -> [String]
execute [] = []
execute xs = undefined
