main :: IO ()
main = do
    num1 <- readLn
    num2 <- readLn
    let list = createList num1 num2
    print (countDeficients list)
    print (countPerfects list)
    print (countAbundants list)
    
createList :: Int -> Int -> [Int]
createList n1 n2
    | n1 <= n2 = [n1 .. n2]
    | otherwise = [n2 .. n1]
    
sumDivisors :: Int -> Int
sumDivisors n = sum [d | d <- [1 .. n-1], n `mod` d == 0]

isPerfect :: Int -> Bool
isPerfect n = sumDivisors n == n

isDeficient :: Int -> Bool
isDeficient n = sumDivisors n < n

isAbundant :: Int -> Bool
isAbundant n = sumDivisors n > n

countPerfects :: [Int] -> Int
countPerfects [] = 0
countPerfects (x:xs)
    | isPerfect x = 1 + countPerfects xs
    | otherwise = countPerfects xs
    
countDeficients :: [Int] -> Int
countDeficients [] = 0
countDeficients (x:xs)
    | isDeficient x = 1 + countDeficients xs
    | otherwise = countDeficients xs
    
countAbundants :: [Int] -> Int
countAbundants [] = 0
countAbundants (x:xs)
    | isAbundant x = 1 + countAbundants xs
    | otherwise = countAbundants xs