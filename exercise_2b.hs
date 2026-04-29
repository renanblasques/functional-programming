main :: IO ()
main = do
    num1 <- readLn
    num2 <- readLn
    let list = createList num1 num2
    let primesList = filterPrimes list
    let result = biggestGap primesList
    print result

createList :: Int -> Int -> [Int]
createList n1 n2
    | n1 <= n2 = [n1 .. n2]
    | otherwise = [n2 .. n1]
    
hasDivider :: Int -> Int -> Bool
hasDivider number divider
    | divider * divider > number = False
    | number `mod` divider == 0 = True
    | otherwise = hasDivider number (nextNumber divider)
    
nextNumber :: Int -> Int
nextNumber divider = (divider + 1)

isPrime :: Int -> Bool
isPrime number
    | number < 2 = False
    | otherwise = not (hasDivider number 2)

filterPrimes :: [Int] -> [Int]
filterPrimes [] = []
filterPrimes (x:xs)
    | isPrime x = x : filterPrimes xs
    | otherwise = filterPrimes xs

biggestGap :: [Int] -> Int
biggestGap [] = 0
biggestGap [_] = 0
biggestGap (x:y:xs) = max (y - x) (biggestGap(y:xs))