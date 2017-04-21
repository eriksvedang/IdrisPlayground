module Eratosthenes

sieve : Int -> List Int -> List Int
sieve n ns = filter (\x => not (mod x n == 0)) ns

primes : List Int -> List Int
primes []      = []
primes (n::ns) = n :: primes (sieve n ns)
