{-TADs-}
module Fila (Fila, filaVazia, ehVazia, insere, remove) where

newtype Fila a = Fila [a]

filaVazia :: Fila a 
filaVazia = Fila []

ehVazia :: Fila a -> Bool
ehVazia (Fila []) = True
ehVazia _ = False

insere :: a -> Fila a -> Fila a
insere x (Fila xs) = Fila (xs ++ [x])

remove :: Fila a -> (a, Fila a)
remove q@(Fila xs)
    | not (ehVazia q) = (head xs, Fila (tail xs))
    | otherwise = error "Não há elemento para remover..." 
