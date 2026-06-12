import System.IO
import Data.List
import Data.Ord
import Data.Maybe

-- Tipo de dado que representa um país
data Country = Country { countryName :: String,
                         confirmed   :: Int,
                         deaths      :: Int,
                         recovery  :: Int,
                         active      :: Int }
    deriving (Show, Read)

-- Main: lê a linha de entrada, abre o arquivo, lê o conteúdo, chama as funções e fecha o arquivo
main :: IO ()
main = do
    line <- getLine
    let [n1, n2, n3, n4] = map read (words line)

    h <- openFile "dados.csv" ReadMode
    content <- hGetContents h
    
    let countries = map parseLine (lines content)
    print (task1 n1 countries)
    print (task2 n2 n3 countries)

    -- mapM_ faz putStrLn para cada elemento da lista
    mapM_ putStrLn (task3 n4 countries)

    hClose h

-- Função auxiliar: parseia uma linha do arquivo CSV e retorna um país
parseLine :: String -> Country
parseLine line =
    Country name (read c) (read d) (read r) (read a)
    where
        [name, c, d, r, a] = stringToList line

-- Função auxiliar: divide uma string em uma lista de strings, separadas por vírgulas
stringToList :: String -> [String]
stringToList [] = [""]
stringToList (x:xs)
    | x == ',' = "" : stringToList xs
    | otherwise = (x : head rest) : tail rest
    where
        rest = stringToList xs

-- Pedido 1: retornar a soma de "Active" de todos os países em que "Confirmed" é maior ou igual a n1
task1 :: Int -> [Country] -> Int
task1 n1 countries =
    sum (map active (filter valid countries))
    where
        valid c = confirmed c >= n1

-- Pedido 2: retornar a soma das "Deaths" dos n3 países com menores valores de "Confirmed",
-- dentro os n2 países com maiores valores de "Active"
task2 :: Int -> Int -> [Country] -> Int
task2 n2 n3 countries =
    sum (map deaths lowestConfirmed)
    where
        topActive =
            take n2 $
            sortBy (flip (comparing active))
            countries

        lowestConfirmed =
            take n3 $
            sortBy (comparing confirmed)
            topActive

-- Pedido 3: retornar os nomes dos n4 países com maiores valores de "Confirmed" em ordem alfabética
task3 :: Int -> [Country] -> [String]
task3 n4 countries =
    sort (map countryName topConfirmed)
    where
        topConfirmed =
            take n4 $
            sortBy (flip (comparing confirmed))
            countries