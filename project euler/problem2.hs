-- Problem 2 sum of even fibonacci numbers below 4000000

-- Recursive definition of fibonacci numbers Fn = Fn-2 + Fn-1 it is far too slow
generateNthFibonacci :: (Integral a) => a -> a
generateNthFibonacci 0 = 1
generateNthFibonacci 1 = 1
generateNthFibonacci x = generateNthFibonacci (x - 2) + generateNthFibonacci (x - 1)



f xs = sum [y | x <- xs, let y = generateNthFibonacci x, y < 4000000 && y `mod` 2 == 0]


-- [1, 1, 2, 3, 5, 8,  13, 21, ...] this is the fibonacci numbers
-- [1, 2, 3, 5, 8, 13, 21, 34, ...] this is just the tail of the above sequence

fibonacci = 1 : 1 : zipWith (+) fibonacci (tail fibonacci)


g = sum[x | x <- take 100 fibonacci, x `mod` 2 == 0 && x < 4000000]