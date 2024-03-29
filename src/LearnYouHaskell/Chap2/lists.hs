boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

removeNonUppercase st = [s | s <- st, s `elem` ['A'..'Z']]

rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b],
    a^2 + b^2 == c^2, a+b+c == 24
    ]