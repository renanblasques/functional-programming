main :: IO ()
main = loop []

loop :: [(String, String)] -> IO ()
loop contact = do
    command <- getLine
    let parts = words command

    case parts of

        ["adicionar", name, phone] -> do
            let newContact = add contact name phone
            loop newContact

        ["buscar", name] -> do
            search contact name
            loop contact

        ["listar"] -> do
            list contact
            loop contact

        ["remover", name] ->
            case filter (sameName name) contact of
                [] -> do
                    putStrLn "Contato nao encontrado."
                    loop contact
                _ -> do
                    let newContact = remove contact name
                    putStrLn "Contato removido."
                    loop newContact

        ["sair"] ->
            putStrLn "Encerrando."

        _ -> do
            putStrLn "Comando invalido."
            loop contact

add :: [(String, String)] -> String -> String -> [(String, String)]
add contact name phone =
    (name, phone) : filter different contact
    where
        different (n, _) = n /= name

search :: [(String, String)] -> String -> IO ()
search [] _ = putStrLn "Contato nao encontrado."

search ((n,t):xs) name
    | n == name = putStrLn (n ++ " - " ++ t)
    | otherwise = search xs name

list :: [(String, String)] -> IO ()
list [] = return ()

list ((n,t):xs) = do
    putStrLn (n ++ " - " ++ t)
    list xs

remove :: [(String, String)] -> String -> [(String, String)]
remove contact name =
    filter different contact
    where
        different (n, _) = n /= name

sameName :: String -> (String, String) -> Bool
sameName name (n, _) = n == name