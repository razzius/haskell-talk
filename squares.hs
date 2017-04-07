join :: String -> [String] -> String
join _ [] = ""
join _ [x] = x
join sep (el : rest) = el ++ sep ++ join sep rest

allNumbers = [1..]

main :: IO ()
main = (putStrLn join ", ") [show (x * x) | x <- allNumbers]
