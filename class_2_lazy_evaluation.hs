main :: IO ()
main = do
  print "HW"
  print $ f 0 s
  -- mesma coisa que print (f 0 s)
  print $ f 1 s
  -- print $ f 2 s dá timeout (precisa calcular s)
  -- print $ g s 1 dá timeout (precisa calcular s pra saber se é 0)
  print $ g 0 0
  -- nesse caso não considera o segundo, porque já viu o primeiro
  -- print $ sum c dá timeout/quebra da heap
  
  print $ take 5 [3, 2, 4, 5, 6, s, 7]
  print $ length [3, 2, 4, 5, 6, s, 7]
  
  print $ sum $ take 10 c
  print $ take 5 uns
  print $ take 20 $ primos
  print $ enesimo 100 primos
  print $ last $ takeWhile (< 1000) primos
  print $ zipWith (+) [3, 4, 2] [2, 6, 8]
  print $ sum $ myZip (*) [5, 4, 2] primos -- produto escalar
  print $ last $ take 100 fibs
  print $ take 10 fibs
  
fibs = 1:1:zipWith (+) fibs (tail fibs)
  
myZip _ [] _ = []
myZip _ _ [] = []
myZip f (x:xs) (y:ys) = f x y:myZip f xs ys


enesimo 0 (x:_) = x
enesimo n (_:xs) = enesimo (n - 1) xs

b = [1..]
c = [x | x <- b, mod x 2 /= 0]

uns = 1:uns
-- lista infinita de 1

primos = p [2..]
  where
    p (x:xs) = x:p [y | y <- xs, mod y x /= 0]

-- p [2..]
-- p [2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13...] = 2:p [3, 5, 7, 9, 11, 13...]
-- p [3, 5, 7, 9, 11, 13...] = 3:p [5, 7, 11, 13...]
-- p [5, 7, 11, 13...] = 5:p [7, 11, 13...]
  
nums = [3, 4, 2, 5, 6, 1]

-- m = 0
m = m + 1

a = [1..100000000000000000]
s = sum a

-- computação não estrita (lazy)
-- só calcula aquilo que realmente precisa (s não é usado)

f 0 _ = 42
f 1 _ = 101
f x y = x + y

g 0 _ = 41
g _ x = x