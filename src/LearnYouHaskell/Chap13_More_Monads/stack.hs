import Control.Monad.State
import Control.Monad

type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)
  
push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop
    pop

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 8

moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip  
    when(a == 100) stackStuff