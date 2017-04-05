data ProductType = LOAN | PPA

data Product = Product { productType :: ProductType
                       , name :: String
                       }

data Quote = Quote { uuid :: String
                   , product_ :: Product
                   }


main :: IO ()
main = let
  loan = Product {name = "Razzi Loan", productType = LOAN}
  quote = Quote {uuid = "abcd", product_ = loan}
  in
  putStrLn (uuid quote) >>
  putStrLn (name $ product_ quote)
