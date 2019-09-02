module ABB (Arv, arvVazia, ehVazia, insere, remove, ehNoNulo, arvEsq, arvDir, infoNo) where

data Arv a = NoNulo | No a (Arv a) (Arv a)

-- Cria arvóre vazia
arvVazia :: Arv a
arvVazia = NoNulo

--Está vazia
ehVazia :: Arv a -> Bool
ehVazia (NoNulo) = True
ehVazia _ = False

-- Nó Nulo?
ehNoNulo :: Arv a -> Bool
ehNoNulo NoNulo = True
ehNoNulo _ = False

-- coleta arvóre a direita
arvEsq :: Arv a -> Arv a
arvEsq (NoNulo) = error "não há arvóre esquerda"
arvEsq (No _ tesq _) = tesq

-- coleta arvóre a direita
arvDir :: Arv a -> Arv a
arvDir (NoNulo) = error "Não há árvore a direita"
arvDir (No _ _ tdir) = tdir

--informação do Nó
infoNo :: Arv a -> a
infoNo NoNulo = error "árvore vazia"
infoNo (No x _ _) = x

--insere na árvore
insere :: Ord a => a -> Arv a -> Arv a
insere  x NoNulo = (No x NoNulo NoNulo)
insere x (No y tesq tdir)
    | x == y = (No x tesq tdir)
    | x > y = No y tesq (insere x tdir)
    | otherwise = No y (insere x tesq) tdir

-- remove da árvore
remove :: Ord a => a -> Arv a -> Arv a
remove  x NoNulo = error "nao há elementos a remover"
remove x (No y tesq tdir)
    | x < y = No y (remove x tesq) tdir
    | x > y = No y tesq (remove x tdir)
    | ehNoNulo tdir = tesq| ehNoNulo tesq = tdir
    | otherwise = una tesq tdir

-- Busca sucessor do nó removido na árvore 
una :: Ord a => Arv a -> Arv a -> Arv a
una tesq tdir = No mini tesq (remove mini tdir)
    where (Just mini) = minArv tdir
        

minArv :: Ord a => Arv a -> Maybe a
minArv t
    | ehNoNulo t = Nothing
    | ehNoNulo (arvEsq t) = Just (infoNo t)
    | otherwise = minArv (arvEsq t)