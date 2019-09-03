module Rest (arvVazia,  ehVazia, ehNoNulo, arvEsq, arvDir, infoNo, 
                insereNo, consultaNo, removeNo ) where 

{- Questão 01
Defina um  tipo  abstrato  de  dados  que  é  uma  árvore  binária  de  busca  
em  que  o  tipo  exportado  é data  Arv (a,b,c) =  NoNulo  |  No (a,b,c) (Arv (a,b,c)) (Arv (a,b,c))
e exporta as   funções: arvVazia,  ehVazia,  ehNoNulo, arvEsq, arvDir,  infoNo, consultaNo, insereNo, removeNo.
A  chave  da árvore  pela qual os nós serão organizados, inseridos, consultado se removidos 
é o primeiro elemento da tupla.
-}
data Arv a = NoNulo | No a (Arv a) (Arv a)
    deriving (Show, Ord, Eq, Read)

type Tuple = (Int, String, Int)

-- Árvore vazia
arvVazia :: Arv Tuple
arvVazia = NoNulo

-- Está vazia ?
ehVazia :: Arv Tuple -> Bool
ehVazia (NoNulo) = True
ehVazia _ = False

-- É nó nulo?
ehNoNulo :: Arv Tuple -> Bool
ehNoNulo NoNulo = True
ehNoNulo _ = False

-- Árvore Esquerda
arvEsq :: Arv Tuple -> Arv Tuple
arvEsq (NoNulo) = error "Não há árvore esquerda"
arvEsq (No _ tesq _) = tesq

--árvore Direita
arvDir :: Arv Tuple -> Arv Tuple 
arvDir (NoNulo) = error "Não há árvore direita"
arvDir (No _ _ tdir) = tdir

--Informação dos nós
infoNo :: Arv Tuple -> Tuple
infoNo NoNulo = error "árvore vazia"
infoNo (No x _ _) = x

--Insere nó
-- Funções auxiliares
fElem :: Tuple -> Int
fElem (x,_,_) = x

sElem :: Tuple -> String
sElem (_, x, _) = x
--
insereNo :: Tuple -> Arv Tuple -> Arv Tuple
insereNo x NoNulo = (No x NoNulo NoNulo)
insereNo x (No y tesq tdir)
    | (fElem x) == (fElem y) = (No x tesq tdir)
    | (fElem x) > (fElem y) = No y tesq (insereNo x tdir)
    | otherwise = No y (insereNo x tesq) tdir

-- Consulta determinado nó a partir da primeira posição da tupla.
-- (função apoio) visita nós da árvore em ordem
emOrdem :: Arv Tuple -> [Tuple]
emOrdem NoNulo = []
emOrdem (No num esq dir) = (emOrdem esq) ++ [num] ++ (emOrdem dir)

consultaNo :: Int -> Arv Tuple -> Tuple
consultaNo i arv = (filter (\x -> i == (fElem x)) (emOrdem arv)) !! 0

-- Remove um Nó específico. 
removeNo :: Int -> Arv Tuple ->[Tuple]
removeNo i arv = (filter (\x -> i /= (fElem x)) (emOrdem arv))


{- Questão 2 
            Informações importantes

            Item        |     Faixa de Códigos
    Bebidas             | 1-50
    Tira-Gostos         | 51-100
    Carnes              | 101-120
    Aves                | 121-140
    Peixes e Marsicos   | 141-160
    Massas              | 161-180
    Acomp. extras       | 181-200
    Sobremesas          | 201-220
-}

{- Questão 2 
Considere  que  o cardápio do restaurante  será  agora  armazenado na  forma  da 
ABB do item 1 onde  a  informação  do  nó  é  do  tipo ItemRest. Escreva as  funções a  seguir 
para manipular o cardápio do restaurante armazenado no sistema.
-}

--(a) Coleta um item no menu, informando seu código.
type Menu = Arv Tuple
type Nome = String
type ItemRest = Tuple
type Codigo = Int

coletaItemMenu :: Menu -> Codigo -> ItemRest
coletaItemMenu m c = consultaNo c m

--(b) Coleta um item no menu, informando seu nome.
--Modificando a função consulta Nó para verificar pelo nome
consultaNomeNo :: String -> Arv Tuple -> Tuple
consultaNomeNo s arv = (filter (\x -> s == (sElem x)) (emOrdem arv)) !! 0

coletaItemMenu2 :: Menu -> Nome-> ItemRest
coletaItemMenu2 m s = consultaNomeNo s m

{-
(c) Atualizar os preços do cardápio de um valor percentual informado. Observe que como o preço é  
sinônimo  de Int,  tudo  deve  ser  expresso  usando  o  tipo Int. Assim,  se  um  item  custa  
1000  e você  deseja  acrescer  de  um  valor  percentual  de  10%, você  deve  informar  10  e  
sua  atualização calculará  o novo valor como o valor antigo acrescido de  div (10*1000) 100.-}

--Estratégia 
--(1) Percorrer árvore (Menu) em ordem. 
pEmOrdem :: Menu -> [Tuple]
pEmOrdem NoNulo = []
pEmOrdem (No num esq dir) = (pEmOrdem esq) ++ [num] ++ (pEmOrdem dir)
--(2) A partir de [Tuple], aplicar map para atualizar todos os preços
upPricesMenu :: [Tuple] -> Int -> [Tuple]
upPricesMenu xs v = [(x, y, z + (div (v * z) 100)) | (x, y, z) <- xs] 
--(3) Dos preços atualizados [Tuple] gerar o novo Menu (árvore)
criarMenu :: [Tuple] -> [Menu]
criarMenu xs = map (\x-> insereNo x NoNulo) xs
--(4) Função final
atualizaPrecosMenu :: Menu -> Int-> [Menu]
atualizaPrecosMenu m p = criarMenu(upPricesMenu(pEmOrdem m) p)


{-(d) Atualizar  os  preços  de  uma  categoria  do  cardápio  de  um  valor  percentual  informado.
Para resolver esta questão você pode precisar de uma função auxiliar, que dado a categoria lhe devolva 
o maior e menor código desta categoria. Observe que a faixa de códigos e as possíveis categorias
são fixas e  não serão atualizadas pelo sistema.Para realizar a questão defina o tipo Categoria 
como um tipo algébrico em que os itens da categoria são os construtores deste tipo.-}

type Categoria = String -- nome da categoria
type TabCategoria = [(String, (Int, Int))] -- tabela itens/faixa de códigos
--Estratégia
categoriaMenu :: TabCategoria -- dados referentes à tabela itens/faixa de códigos
categoriaMenu = [
                    ("Bebidas", (1, 50)), ("Tira-gostos",(51, 100)), ("Carnes", (101, 120))
                    ,("Aves", (121, 140)), ("Peixes e Mariscos", (141, 160)), ("Massas", (161, 180))
                    ,("Acompanhamentos extras", (181, 200)), ("Sobremesas", (201, 220))
                ]
-- (1) Dada uma categoria, retorna o maior e o menor valor
catMaiorMenorCod :: Categoria -> (Codigo, Codigo)
catMaiorMenorCod nm = snd((filter (\x-> nm == fst x) categoriaMenu) !! 0)

-- (2) Verifica se o nó pertence a uma determinada categoria. 
verifCateg :: (Codigo, Codigo) -> Menu -> [Tuple]
verifCateg t m = filter (\x -> (fElem x >= fst t) && (fElem x <= snd t)) (emOrdem m) 

--Função Final    
atualizaPrecosCat:: Menu -> Categoria -> Int -> [Menu]
atualizaPrecosCat menu cat vlr =  criarMenu (upPricesMenu (verifCateg (catMaiorMenorCod cat) menu) vlr)


--(e) Inserir um  conjunto  de itens  novos no  menu. Sua  função  só  deve  inserir  se  o item ainda  
--não existir no menu.(obs)
insereItemMenu :: Menu -> [Tuple]-> Menu
insereItemMenu menu [] = menu
insereItemMenu menu (x:xs) 
    | x == (consultaNo (fElem x) menu) = error "nó existente"
    | otherwise = insereNo x (insereItemMenu menu xs)

--(f) Remove um conjunto de itens do menu.Sua função só deve remover se o item existir no menu.
--removeItemMenu :: Menu -> [Tuple] -> Menu



--(g) Coletar todos os itens de uma dada categoria.
coletaItensCat :: Menu -> Categoria -> [Tuple]
coletaItensCat menu cat = filter (\x -> (fElem x >= fst(catMaiorMenorCod cat)) 
                            && (fElem x <= snd(catMaiorMenorCod cat) )) (emOrdem menu) 

--(h) Contar quantos itens existem no menu.
totalMenu :: Menu -> Int
totalMenu menu  = length (emOrdem menu)

--(i) Listar quantos itens existem no menu, por categoria.
--type NomeCat = String
listItensCat :: Menu -> Categoria -> [Tuple]
listItensCat menu cat = 

{-
totalCat:: Menu->[(NomeCat,Int)]
(j)Coletar onome do item mais barato e o mais caro, por categoria.
itemCaroBarat :: Menu->Categoria -> [(NomeCat,(Nome,Nome))]
-}

menu :: Arv Tuple
menu = (No (122,"Frango", 3200)
            (No (101, "Picanha",4000) NoNulo NoNulo) NoNulo)