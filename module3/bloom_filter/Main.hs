module Main where

import Data.Array.Unboxed (UArray, (//), (!), array, indices)
import Data.Bits (shiftL)
import Data.Bool (bool)

-- list of prime numbers
primes :: [Int]
primes = sieve [2..]

-- sieve of Eratosthenes
sieve :: [Int] -> [Int]
sieve (x:xs) = let multof x n = n `rem` x == 0
                in x : (sieve (filter (not . multof x) xs))

-- 31st Mersenne number
mersenne31 :: Int
mersenne31 = (2 `shiftL` 31) - 1

-- Bloom filter
data BloomFilter a = BloomFilter [a -> Int] (UArray Int Bool)

instance Show (BloomFilter a) where
  show (BloomFilter _ arr) = [bool '0' '1' (arr ! i) | i <- indices arr]

create :: Int -> [a -> Int] -> BloomFilter a
create m hashes = let arr = array (0, m - 1) [(i, False) | i <- [0..m-1]]
                   in BloomFilter hashes arr

insert :: a -> BloomFilter a -> BloomFilter a
insert x (BloomFilter hashes arr) = let arr' = arr // [(i, True) | i <- (hashes <*> pure x)]
                                     in BloomFilter hashes arr'

search :: a -> BloomFilter a -> Bool
search x (BloomFilter hashes arr) = all id [arr ! i | i <- (hashes <*> pure x)]

main :: IO ()
main = putStrLn "Hello world"
