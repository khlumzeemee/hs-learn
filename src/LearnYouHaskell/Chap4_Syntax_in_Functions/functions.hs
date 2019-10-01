lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER 7!!"
lucky x = "Sorry, you're out of luck pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "can't call head on an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x,y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list is long! The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (x:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "underweight"
    | bmi <= 25.0 = "normal"
    | bmi <= 30.0 = "overweight"
    | otherwise   = "obese"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | weight / height ^ 2 <= 18.5 = "underweight"
    | weight / height ^ 2 <= 25.0 = "normal"
    | weight / height ^ 2 <= 30.0 = "overweight"
    | otherwise   = "obese"

max' :: (Ord a) => a -> a -> a
max' x y
    | x > y     = x
    | otherwise = y

myCompare :: (Ord a) => a -> a -> Ordering
x `myCompare` y
    | x > y     = GT
    | x == y    = EQ
    | otherwise = LT

bmiTell'' :: (RealFloat a) => a -> a -> String
bmiTell'' weight height
    | bmi <= skinny = "underweight"
    | bmi <= normal = "normal"
    | bmi <= fat = "overweight"
    | otherwise   = "obese"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

initials :: String -> String -> String
initials firstName lastName = [f] ++ "." ++ [l] ++ "."
    where (f:_) = firstName
          (l:_) = lastName

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w,h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in  sideArea + 2 * topArea

calcBmis' :: (RealFloat a) => [(a,a)] -> [a]
calcBmis' xs = [bmi |(w,h) <- xs, let bmi = w / h ^ 2]

describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty"
          what [x] = "a singleton list"
          what xs = "a longer list"