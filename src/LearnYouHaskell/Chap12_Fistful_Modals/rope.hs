type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft b (l,r)
    | abs ((l+b) - r) >= 4 = Nothing
    | otherwise = Just (l + b, r)

landRight :: Birds -> Pole -> Maybe Pole
landRight b (l,r)
    | abs ((r+b) - l) >= 4 = Nothing
    | otherwise = Just (l, r + b)

x -: f = f x