module Tools.Utilities where

import System.IO

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = foldr (\x seen -> if x `elem` seen then seen else x : seen) []

programsPrefixPath :: String
programsPrefixPath = "programs/"

readInput :: IO String
readInput = do
    putStr "> Insert source file name: "
    hFlush stdout
    programName <- getLine
    input <- readF (programsPrefixPath ++ programName)
    return input

readF :: String -> IO String
readF fileName = do
    inh <- openFile fileName ReadMode
    prog <- readLoop inh
    hClose inh
    return prog

readLoop :: Handle -> IO [Char]
readLoop inh = do
    ineof <- hIsEOF inh
    if ineof
        then return []
        else do
            x <- hGetLine inh
            xs <- readLoop inh
            return (x ++ xs)

bottomString :: String
bottomString = "┴"

-- filtra finche non trovi il primo che verifica il predicato
filterFirst :: (a -> Bool) -> [a] -> Maybe a
filterFirst _ [] = Nothing
filterFirst p (x:xs)
    | p x       = Just x
    | otherwise = filterFirst p xs

-- filtra finche non trovi un valore che nega il predicato
filterSince :: (a -> Bool) -> [a] -> [a]
filterSince _ [] = []
filterSince p (x:xs)
    | p x       = x:filterSince p xs
    | otherwise = []