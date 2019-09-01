-- Exemplos de IO

-- IO que lê um inteiro
getInt :: IO Int
getInt = do ln <- getLine
            return (read ln :: Int)

----------------------------------

-- somaInts n: lê n inteiros e retorna a soma deles.
somaInts :: Int -> IO Int 
somaInts n =
   if n == 0
   then return 0
   else do m <- getInt
           p <- somaInts (n-1)
           return (m+p)

main1 :: IO ()                   
main1 = do n <- getInt
           m <- somaInts n
           putStr $ show m ++  "\n"


-- somaInts n: lê n inteiros e retorna a soma deles.
-- A soma é acumulada no segundo argumento
somaInts2 :: Int -> Int -> IO Int 
somaInts2 n res =
   if n <=0
   then return res
   else do m <- getInt
           somaInts2 (n-1) (res+m)

main2 :: IO ()                   
main2 = do n <- getInt
           m <- somaInts2 n 0
           putStr $ show m ++ "\n"
   
----------------------------------------

-- getList n: lê n inteiros. A ação devolve a lista
-- de números lidos positivos
getList :: Int -> IO [Int]
getList n = do
   if n==0
   then return []
   else do
     x <- getInt
     xs <- getList (n-1)
     return (x:xs)

main3 :: IO ()
main3 = do n <- getInt
           xs <- getList n
           putStr $ show (filter (>0) xs) ++ "\n"

-----------------------------------------

-- getListReversed n: lê n inteiros. A ação devolve a lista
-- de números lidos, na ordem inversa da leitura
getListReversed :: Int -> IO [Int]   
getListReversed n = 
   if n==0
   then return []
   else do
     xs <- getListReversed (n-1)
     x <- getInt
     return (x:xs)

main4 :: IO ()
main4 = do n <- getInt
           xs <- getListReversed n
           putStr $ show xs ++ "\n"
  
----------------------------------------


-- getListAcc n []: lê n inteiros. A ação devolve a lista
-- de números lidos, na ordem inversa da leitura. Utiliza
-- o segundo argumento como um "acumulador" em que se colocam
-- os números lidos
getListAcc :: Int -> [Int] -> IO [Int]
getListAcc n xs = do
   if n==0
   then return xs
   else do
     x <- getInt
     getListAcc (n-1) (x:xs) 

main5 :: IO ()
main5 = do n <- getInt
           xs <- getListAcc n []
           putStr $ show xs ++ "\n"
   
----------------------------------------

-- leEOrdena lê uma sequência de inteiros até
-- o ingresso de 0. Devolve a lista com os números lidos,
-- ordenada ascendentemente.   
leEOrdena :: IO [Int]
leEOrdena = do
   n <- getInt
   if n == 0
   then return []
   else do
      ns <- leEOrdena
      return (ins n ns)

-- ins é a função usada na ordenação por inserção.
-- Se xs está ordenado, ins x xs retorna
-- a lista ordenada, acrescentando x a xs.     
ins x [] = [x]
ins x (y:ys) 
   | x <= y    = x : y : ys
   | otherwise = y : ins x ys   
