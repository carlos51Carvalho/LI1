-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2018li1g146 where

import LI11819
import Data.List

-- * FunÃ§Ãµes nÃ£o-recursivas.

-- | Um 'Vetor' Ã© uma 'Posicao' em relaÃ§Ã£o Ã  origem.
type Vetor = Posicao
-- ^ <<http://oi64.tinypic.com/mhvk2x.jpg vetor>>

-- ** FunÃ§Ãµes sobre vetores

-- *** FunÃ§Ãµes gerais sobre 'Vetor'es.

-- | Soma dois 'Vetor'es.
somaVetores :: Vetor -> Vetor -> Vetor
somaVetores (x1,y1) (x2,y2) = (x1 + x2, y1 + y2) 

-- | Subtrai dois 'Vetor'es.
subtraiVetores :: Vetor -> Vetor -> Vetor
subtraiVetores (x1,y1) (x2,y2) = (x1 - x2, y1 - y2)

-- | Multiplica um escalar por um 'Vetor'.
multiplicaVetor :: Int -> Vetor -> Vetor
multiplicaVetor x (a,b) = (x*a , x*b)

-- | Roda um 'Vetor' 90º no sentido dos ponteiros do relógio, alterando a sua direção sem alterar o seu comprimento (distância à origem).
--
-- <<http://oi65.tinypic.com/2j5o268.jpg rodaVetor>>
rodaVetor :: Vetor -> Vetor
rodaVetor (x , z) = (z , -x)

-- | Espelha um 'Vetor' na horizontal (sendo o espelho o eixo vertical).
--
-- <<http://oi63.tinypic.com/jhfx94.jpg inverteVetorH>>
inverteVetorH :: Vetor -> Vetor
inverteVetorH (x , y) = (x , -y)

-- | Espelha um 'Vetor' na vertical (sendo o espelho o eixo horizontal).
--
-- <<http://oi68.tinypic.com/2n7fqxy.jpg inverteVetorV>>
inverteVetorV :: Vetor -> Vetor
inverteVetorV (x , y) = (-x , y)

-- *** Funções do trabalho sobre 'Vetor'es.

-- | Devolve um 'Vetor' unitário (de comprimento 1) com a 'Direcao' dada.
direcaoParaVetor :: Direcao -> Vetor
direcaoParaVetor x | x == C = (-1,0)
                   | x == E = (0,-1)
                   | x == D = (0,1)
                   | otherwise = (1 , 0)
-- ** Funções sobre listas

-- *** Funções gerais sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Verifica se o indice pertence à lista.
eIndiceListaValido :: Int -> [a] -> Bool
eIndiceListaValido x z = if x < 0 || x > (length z-1) then False else True 

-- ** Funções sobre matrizes.

-- *** Funções gerais sobre matrizes.

-- | Uma matriz é um conjunto de elementos a duas dimensões.
--
-- Em notação matemática, é geralmente representada por:
--
-- <<https://upload.wikimedia.org/wikipedia/commons/d/d8/Matriz_organizacao.png matriz>>
type Matriz a = [[a]]

-- | Calcula a dimensão de uma matriz.
--
-- __NB:__ Note que não existem matrizes de dimensão /m * 0/ ou /0 * n/, e que qualquer matriz vazia deve ter dimensão /0 * 0/.
dimensaoMatriz :: Matriz a -> Dimensao 
dimensaoMatriz (a:as) | length a > 0 = (length (a:as) , length a)
                      | otherwise = (0,0)
dimensaoMatriz [] = (0,0)

-- | Verifica se a posição pertence à matriz.
ePosicaoMatrizValida :: Posicao -> Matriz a -> Bool 
ePosicaoMatrizValida (x,y) [] = False
ePosicaoMatrizValida (x,y) (h:hs) = x <= length (h:hs)-1 && y <= length h-1 && x >= 0 && y >= 0

-- | Verifica se a posição está numa borda da matriz.
eBordaMatriz :: Posicao -> Matriz a -> Bool
eBordaMatriz _ [] = False
eBordaMatriz (x,y) (h:hs) = x == length (h:hs)-1 || y == length h-1 || x == 0 || y == 0  

-- *** Funções do trabalho sobre matrizes.

-- | Converte um 'Tetromino' (orientado para cima) numa 'Matriz' de 'Bool'.
--
-- <<http://oi68.tinypic.com/m8elc9.jpg tetrominos>>
tetrominoParaMatriz :: Tetromino -> Matriz Bool
tetrominoParaMatriz I = [[False,True,False,False],[False,True,False,False],[False,True,False,False],[False,True,False,False]]
tetrominoParaMatriz J = [[False,True,False],[False,True,False],[True,True,False]]
tetrominoParaMatriz L = [[False,True,False],[False,True,False],[False,True,True]]
tetrominoParaMatriz O = [[True,True],[True,True]]
tetrominoParaMatriz S = [[False,True,True],[True,True,False],[False,False,False]]
tetrominoParaMatriz T = [[False,False,False],[True,True,True],[False,True,False]]
tetrominoParaMatriz Z = [[True,True,False],[False,True,True],[False,False,False]]

-- * Funções recursivas.

-- ** Funções sobre listas.
--
-- Funções não disponíveis no 'Prelude', mas com grande utilidade.

-- | Devolve o elemento num dado índice de uma lista.
encontraIndiceLista :: Int -> [a] -> a
encontraIndiceLista 0 (h:hs) = h
encontraIndiceLista x [] = error "The index is too Big."
encontraIndiceLista x (h:hs) = encontraIndiceLista (x-1) hs

-- | Modifica um elemento num dado índice.
--
-- __NB:__ Devolve a própria lista se o elemento não existir.
atualizaIndiceLista :: Int -> a -> [a] -> [a]
atualizaIndiceLista x y (h:hs) | x > ((length (h:hs))-1) = (h:hs)
                               | x <= ((length(h:hs))-1) && x > 0 = h: atualizaIndiceLista (x-1) y hs
                               | x == 0 = y : hs

-- ** Funções sobre matrizes.

-- | Roda uma 'Matriz' 90º no sentido dos ponteiros do relógio.
--
-- <<http://oi68.tinypic.com/21deluw.jpg rodaMatriz>>
rodaMatriz :: Matriz a -> Matriz a
rodaMatriz ([]:hs) = []
rodaMatriz (h:hs) = reverse (cabeca (h:hs)) : rodaMatriz (aux1 (h:hs))
rodaMatriz [] = []

aux1 :: Matriz a -> Matriz a
aux1 [] = []
aux1 (h:hs) = tail h : aux1 hs

cabeca :: Matriz a -> [a]
cabeca [] = []
cabeca (h:hs) = (head h) : cabeca hs

-- | Inverte uma 'Matriz' na horizontal.
--
-- <<http://oi64.tinypic.com/iwhm5u.jpg inverteMatrizH>>
inverteMatrizH :: Matriz a -> Matriz a
inverteMatrizH [] = []
inverteMatrizH (h:hs) = reverse h : inverteMatrizH hs

-- | Inverte uma 'Matriz' na vertical.
--
-- <<http://oi64.tinypic.com/11l563p.jpg inverteMatrizV>>
inverteMatrizV :: Matriz a -> Matriz a
inverteMatrizV [] = []
inverteMatrizV (h:hs) = inverteMatrizV hs ++ [h]

-- | Cria uma nova 'Matriz' com o mesmo elemento.
criaMatriz :: Dimensao -> a -> Matriz a
criaMatriz (x,y) a = aux3 x (aux2 y a)

aux2 :: Int -> a -> [a]
aux2 0 a = []
aux2 x a = a : aux2 (x-1) a

aux3:: Int -> [a] -> Matriz a
aux3 0 a = []
aux3 x a = a : aux3 (x-1) a

-- | Devolve o elemento numa dada 'Posicao' de uma 'Matriz'.
encontraPosicaoMatriz :: Posicao -> Matriz a -> a
encontraPosicaoMatriz (x,y) (h:hs) | x > 0 = encontraPosicaoMatriz ((x-1),y) hs
                                   | x == 0 = aux4 y h


aux4 :: Int -> [a] -> a
aux4 y (h:hs) | y > 0 = aux4 (y-1) hs
              | y == 0 = h


-- | Modifica um elemento numa dada 'Posicao'
--
-- __NB:__ Devolve a própria 'Matriz' se o elemento não existir.
atualizaPosicaoMatriz :: Posicao -> a -> Matriz a -> Matriz a
atualizaPosicaoMatriz (x,y) a (h:hs) | x > 0 = h : atualizaPosicaoMatriz ((x-1),y) a hs
                                     | x == 0 = aux5 y a h : hs

aux5 :: Int -> a -> [a] -> [a]
aux5 y a (h:hs) | y > 0 = h : aux5 (y-1) a hs
                | y == 0 = a : hs

