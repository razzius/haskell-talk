data ProductType = Loan | PPA | Lease

data Quote = Quote { uuid :: String
                   , productType :: ProductType
                   , initialPayment :: Float
                   }


upfrontPayment (Quote {productType = Loan, initialPayment = initial}) = initial
upfrontPayment (Quote {productType = PPA}) = 0
upfrontPayment (Quote {productType = Lease}) = 0


main = putStrLn $ "Upfront payment of quote is: " ++ show upfrontCost
  where upfrontCost = upfrontPayment quote
        quote = Quote {uuid = "123", productType = Loan, initialPayment = 1000 }
