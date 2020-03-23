maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100   

data Tree = Null | Leaf Int | Node Int Tree Tree deriving Show

printInOrder Null= ""
printInOrder (Leaf x) = show x
printInOrder (Node x left right) = printInOrder left ++ " " ++ (show x) ++ " " ++ (printInOrder right)

tsize Null = 0
tsize (Leaf _) = 1
tsize(Node _ left right) = 1+ (tsize left) + (tsize right)

sumt Null = 0
sumt (Leaf x) = x
sumt (Node x left right) = x +(sumt left) + (sumt right)
