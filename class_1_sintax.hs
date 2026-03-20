main :: IO ()
main = do
  print "HW"
  print (sinal 42)
  print (baskara 1 0 (-2))
  print nums
  print nums2
  print (myLen nums)
  print (mySum nums)
  print (myProd nums)
  print (myFilter (<5) nums)
  print ((<5) 6)
  print (mySum (myFilter (<5) nums))
  print (foldr (+) 0 nums)
  print (foldr (*) 1 nums)
  print (minhaSoma nums)
  print ("X" ++ "Y")
  print (foldr (++) "" strs)
  
strs = ["Adenilso", "da", "Silva", "Simao"]
  
minhaSoma l = foldr (+) 0 l
--minhaSoma = foldr (+) 0
  
sinal x
  | x < 0 = -1
  | x == 0 = 0
  | otherwise = 1
  
baskara a b c
  | delta < 0 = []
  | delta == 0 = [x]
  | otherwise = [x', x'']
  where
    delta = b^2 - 4 * a * c
    x = (-b) / (2 * a)
    x' = (-b + sqdelta) / (2 * a)
    x'' = (-b - sqdelta) / (2 * a)
    sqdelta = sqrt delta

nums = [4, 3, 6, 1, 2, 5]

nums2 = (4:(3:(6:(1:(2:(5:[]))))))

myLen [] = 0
myLen (_:xs) = 1 + myLen xs
-- o underscore indica que não há necessidade 
-- myLen (x:xs) = 1 + myLen xs

-- myLen [4, 3, 6, 1, 2, 5] = 1 + myLen [3, 6, 1, 2, 5]
-- myLen [3, 6, 1, 2, 5] = 1 + myLen [6, 1, 2, 5]
-- myLen [6, 1, 2, 5] = 1 + myLen [1, 2, 5]
-- myLen [1, 2, 5] = 1 + myLen [2, 5]
-- myLen [2, 5] = 1 + myLen [5]
-- myLen [5] = 1 + myLen []
-- myLen [] = 0

mySum [] = 0
mySum (x:xs) = x + mySum xs

-- mySum [4, 3, 6, 1, 2, 5] = 1 + mySum [3, 6, 1, 2, 5]
-- mySum [3, 6, 1, 2, 5] = 1 + mySum [6, 1, 2, 5]
-- mySum [6, 1, 2, 5] = 1 + mySum [1, 2, 5]
-- mySum [1, 2, 5] = 1 + mySum [2, 5]
-- mySum [2, 5] = 1 + mySum [5]
-- mySum [5] = 1 + mySum []
-- mySum [] = 0

myProd [] = 1
myProd (x:xs) = x * myProd xs

myMap _ [] = []
myMap f (x:xs) = f x:myMap f xs
myMap f (x:xs) = (f x):(myMap f xs)

myFilter t [] = []
myFilter t (x:xs)
  | t x = x:(myFilter t xs)
  | otherwise = myFilter t xs
  
{-
myFilter _ [] = []
myFilter t (x:xs)
  | t x = x:ts
  | otherwise = ts
  where
    ts = myFilter t xs
-}
  
-- myFilter (<5) [6, 1, 2, 5] = myFilter (<5) [1, 2, 5]
-- myFilter (<5) [1, 2, 5] = 1:myFilter (<5) [2, 5]
-- myFilter (<5) [2, 5] = 2:myFilter (<5) [5]
-- myFilter (<5) [5] = myFilter (<5) []
-- myFilter (<5) [] = []