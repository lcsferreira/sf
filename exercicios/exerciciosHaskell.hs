multLista :: Int -> [Int] -> [Int]
multLista n [] = []

multLista n (x:xs) = (n*x) : multLista n xs
