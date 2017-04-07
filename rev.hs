rev [] = []
rev (x : xs) = rev xs ++ [x]


main = print $ rev [1,2,3]

