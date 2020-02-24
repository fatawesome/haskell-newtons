module Lib
    ( someFunc,
      root
    ) where


calculateCurrentRoot
  :: Fractional a
  => (a -> a)
  -> (a -> a)
  -> a
  -> a
calculateCurrentRoot f f' xn = xn - f xn / f' xn

calculateError :: Fractional a => a -> a -> a
calculateError x1 x2 = abs (x2 - x1)

takeFirst :: [a] -> a
takeFirst (x:xs) = x

-- | Find a root of an equation
--
-- f(x) = 0
--
-- using Newton's method
root
  :: Fractional a
  => Int -- ^ maximum number of iterations
  -> (a -> Bool) -- ^ an acceptable error (precision)
  -> (a -> a) -- ^ function f to find 0 for
  -> (a -> a) -- ^ derivative of f
  -> a -- ^ initial guess
  -> Maybe (Int, a) -- ^ Number of iterations and root, if found
root max_iters errComparator f f' x0
  | null goodError = Nothing
  | otherwise = getAnswer (take 1 goodError)
  where
    getAnswer ((idx, (xn, _)):xs) = Just (idx, xn)
    goodError = dropWhile (\(idx, (xn, errn)) -> not (errComparator errn)) maxLengthSeq
    maxLengthSeq = take max_iters infSequence
    infSequence = zip [-1..] (iterate nextPair (x0, 1000000.0))
    nextPair (xn, _) = (nextX xn, errN (nextX xn) xn)
    errN xn xn1 = abs (xn1 - xn)
    nextX xn = xn - f xn / f' xn


someFunc :: IO ()
someFunc = putStrLn "someFunc"
