-- join "_" ["a", "b"] -> "a_b"
--
join _ [] = ""
join sep [element] = element
join sep (element : rest) = element ++ sep ++ join sep rest


main = putStrLn commaSquares
  where
    commaSquares = join ", " squares
    squares = map (show . (\x -> x * x)) numbers
    numbers = [1..10]
