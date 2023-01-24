--1
add10toall :: [Int] -> [Int]
add10toall list2 = [list1 + 10 | list1 <- list2]

--2
multN :: Int -> [Int] -> [Int]
multN n l2 = [l1 * n | l1 <- l2]

--3
multN' :: Int -> [Int] -> [Int]
multN' n y = map (\x -> n * x) y

--4
applyExpr :: [Int] -> [Int]
applyExpr b = [3 * a + 2 | a <- b]

--5
applyExpr' :: [Int] -> [Int]
applyExpr' z = map (\x -> 3*x+2) z

--6
addSuffix :: String -> [String] -> [String]
addSuffix suf r = [str ++ suf | str <- r]

--7
selectgt5 :: [Int] -> [Int]
selectgt5 j = [i | i <- j, i < 5]

--8
sumOdds :: [Int] -> Int
sumOdds l = sum [x | x <- l, odd x]

--9

sumOdds' :: [Int] -> Int
sumOdds' y = sum (filter (\x -> mod x 2 /= 0) y)

--10
selectExpr :: [Int] -> [Int]
selectExpr m = [n | n <- m, n >= 20, n <= 50, even n]

--11
countShorts :: [String] -> Int
countShorts qtd = length [entrada | entrada <- qtd, length entrada < 5]

--12
calcExpr :: [Float] -> [Float]
calcExpr y = [x^2/2 | x <- y, x > 10]

--13
trSpaces :: String -> String
trSpaces t = [if s == ' ' then '-' else s | s <- t]

--14
selectSnd :: [(Int,Int)] -> [Int]
selectSnd saida = [ b | (a,b) <- saida]

--15
dotProd :: [Int] -> [Int] -> Int
dotProd a b = sum [(x * y) | (x,y) <- zip a b]