-- | Este módulo define funções comuns da Tarefa 1 do trabalho prático.
module Tarefa1_2018li1g146 where

import LI11819
import Tarefa0_2018li1g146

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é uma sequência de 'Instrucoes'.
testesT1 :: [Instrucoes]
testesT1 = [[(Move C),(Move D), (Move E), (Move B), Roda, MudaTetromino, MudaParede, Desenha],
            [MudaTetromino, Desenha, (Move D), Desenha],
            [(Move C), (Move D), MudaTetromino, (Move E), Desenha],
            [Desenha, (Move C), (Move C), (Move D), MudaTetromino, Desenha],
            [(Move C), (Move D), Desenha, MudaTetromino, (Move C), Desenha],
            [MudaTetromino, Roda, Roda, MudaParede, MudaTetromino, Roda, Desenha],
            [Roda, (Move C), MudaParede, Desenha],
            [Roda, MudaParede, Desenha],
            [MudaParede, Roda, (Move D),Desenha]]



-- * Funções principais da Tarefa 1.

-- | Aplica uma 'Instrucao' num 'Editor'.
--
--    * 'Move' - move numa dada 'Direcao'.
--
--    * 'MudaTetromino' - seleciona a 'Peca' seguinte (usar a ordem léxica na estrutura de dados),
--       sem alterar os outros parâmetros.
--
--    * 'MudaParede' - muda o tipo de 'Parede'.
--
--    * 'Desenha' - altera o 'Mapa' para incluir o 'Tetromino' atual, sem alterar os outros parâmetros.
instrucao :: Instrucao -- ^ A 'Instrucao' a aplicar.
          -> Editor    -- ^ O 'Editor' anterior.
          -> Editor    -- ^ O 'Editor' resultante após aplicar a 'Instrucao'.

instrucao (Move C) (Editor (l,c) d t w m ) = (Editor (l-1,c) d t w m )
instrucao (Move D) (Editor (l,c) d t w m ) = (Editor (l,c+1) d t w m )
instrucao (Move B) (Editor (l,c) d t w m ) = (Editor (l+1,c) d t w m )
instrucao (Move E) (Editor (l,c) d t w m ) = (Editor (l,c-1) d t w m )


instrucao Roda (Editor p C t w m ) = Editor p D t w m
instrucao Roda (Editor p D t w m ) = Editor p B t w m
instrucao Roda (Editor p B t w m ) = Editor p E t w m
instrucao Roda (Editor p E t w m ) = Editor p C t w m

instrucao MudaTetromino (Editor p d I w m ) = Editor p d J w m
instrucao MudaTetromino (Editor p d J w m ) = Editor p d L w m
instrucao MudaTetromino (Editor p d L w m ) = Editor p d O w m
instrucao MudaTetromino (Editor p d O w m ) = Editor p d S w m
instrucao MudaTetromino (Editor p d S w m ) = Editor p d T w m
instrucao MudaTetromino (Editor p d T w m ) = Editor p d Z w m
instrucao MudaTetromino (Editor p d Z w m ) = Editor p d I w m

instrucao MudaParede (Editor p d t Indestrutivel m ) = Editor p d t Destrutivel m
instrucao MudaParede (Editor p d t Destrutivel m ) = Editor p d t Indestrutivel m

instrucao Desenha (Editor p d t w mapa) = Editor p d t w (desenha p d t w mapa)

-- | Produz uma Matriz de Bools dependendo de uma direcao e de um tipo de Tetromino.
direcaoTetromino:: Direcao       -- ^ A 'Direcao' a aplicar ao Tetromino.
                -> Tetromino     -- ^ O tipo de 'Tetromino'. 
                -> Matriz Bool   -- ^ A 'Matriz' equivalente a um dado 'Tetromino' numa determinada 'Direcao'.
direcaoTetromino C t = tetrominoParaMatriz t
direcaoTetromino D t= rodaMatriz (tetrominoParaMatriz t)
direcaoTetromino B t= rodaMatriz (rodaMatriz (tetrominoParaMatriz t))
direcaoTetromino E t= rodaMatriz (rodaMatriz (rodaMatriz (tetrominoParaMatriz t)))


-- | Recebe um elemento tipo 'Bool' e caso 'True' atualiza uma 'Posicao' de um determinado 'Mapa' para um tipo de 'Parede'. 
atualizaUmaPosicaoMapa:: Posicao -- ^ A 'Posicao' que vai ser atualizada.
                      -> Bool    -- ^ Um elemento 'Bool'. 
                      -> Parede  -- ^ O tipo de 'Parede' para atualizar.
                      -> Mapa    -- ^ O 'Mapa' inicial.
                      -> Mapa    -- ^ O 'Mapa' resultante.
atualizaUmaPosicaoMapa (l,c) t w m  |t == False = m 
                                    |otherwise = atualizaPosicaoMatriz (l,c) (Bloco w) m 


-- | Recebe uma lista de elementos 'Bool' e caso 'True' e atualiza as 'Posicoes' a eles respetiva num 'Mapa' para o tipo de 'Parede' 
atualizaLinhaMapa:: Posicao -- ^ A primeira 'Posicao' a ser testada.
                 -> [Bool]  -- ^ A lista de 'Bool'.
                 -> Parede  -- ^ O tipo de 'Parede' para atualizar.
                 -> Mapa    -- ^ O 'Mapa' inicial.
                 ->Mapa     -- ^ O 'Mapa' resultante.
atualizaLinhaMapa _ [] w m = m
atualizaLinhaMapa (l,c) (h:t) w m = atualizaLinhaMapa (l, c+1) t w (atualizaUmaPosicaoMapa (l,c) h w m)

-- | Recebe uma lista de listas de 'Bool' e caso True atualiza a 'Posicao' a eles respetiva num 'Mapa' para o tipo de 'Parede'
atualizaMapa:: Posicao   -- ^ A primeira 'Posicao' a testada. 
            -> [[Bool]]  -- ^ A lista de listas de 'Bool'.
            -> Parede    -- ^ O tipo de 'Parede' para atualizar.
            -> Mapa      -- ^ O 'Mapa' inicial.
            -> Mapa      -- ^ O 'Mapa' resultante.       
atualizaMapa (l,c) [] w m = m
atualizaMapa (l,c) (h:t) w m = atualizaMapa (l+1,c) t w (atualizaLinhaMapa (l,c) h w m ) 

-- | Recebe um 'Tetromino' e uma 'Direcao' e de acordo com a 'Matriz Bool' por estes formada vai atualizar a 'Posicao' respetiva no 'Mapa' caso 'True' para o tipo de 'Parede' 
desenha:: Posicao    -- ^ A primeira 'Posicao' a ser testada.
       -> Direcao    -- ^ A 'Direcao' a aplicar ao 'Tetromino'
       -> Tetromino  -- ^ O tipo de 'Tetromino'
       -> Parede     -- ^ O tipo de 'Parede' para atualizar.
       -> Mapa       -- ^ O 'Mapa' inicial.
       -> Mapa       -- ^ O 'Mapa' resultante.
desenha (l,c) d tetro  w m  = atualizaMapa (l,c) (direcaoTetromino d tetro) w m



-- | Aplica uma sequência de 'Instrucoes' num 'Editor'.
--
-- __NB:__ Deve chamar a função 'instrucao'.
instrucoes :: Instrucoes -- ^ As 'Instrucoes' a aplicar.
           -> Editor     -- ^ O 'Editor' anterior.
           -> Editor     -- ^ O 'Editor' resultante após aplicar as 'Instrucoes'.
instrucoes [] e = e
instrucoes (h:t) e = instrucoes t (instrucao h e) 

-- | Cria um 'Mapa' inicial com 'Parede's nas bordas e o resto vazio.
mapaInicial :: Dimensao -- ^ A 'Dimensao' do 'Mapa' a criar.
            -> Mapa     -- ^ O 'Mapa' resultante com a 'Dimensao' dada.
mapaInicial (l,c) = let pl = replicate c (Bloco Indestrutivel)
                        lm = ((Bloco Indestrutivel):(replicate (c-2) Vazia)) ++ [Bloco Indestrutivel]
                        lsm = replicate (l-2) lm
                    in ((pl:lsm)++[pl])


-- | Cria um 'Editor' inicial.
--
-- __NB:__ Deve chamar as funções 'mapaInicial', 'dimensaoInicial', e 'posicaoInicial'.
editorInicial ::Instrucoes  -- ^ Uma sequência de 'Instrucoes' de forma a poder calcular a  'dimensaoInicial' e a 'posicaoInicial'.
              -> Editor      -- ^ O 'Editor' inicial, usando a 'Peca' 'I' 'Indestrutivel' voltada para 'C'.
editorInicial inst = Editor (posicaoInicial inst) C I Indestrutivel (mapaInicial (dimensaoInicial inst))

-- | Constrói um 'Mapa' dada uma sequência de 'Instrucoes'.
--
-- __NB:__ Deve chamar as funções 'Instrucoes' e 'editorInicial'.
constroi :: Instrucoes -- ^ Uma sequência de 'Instrucoes' dadas a um 'Editor' para construir um 'Mapa'.
         -> Mapa       -- ^ O 'Mapa' resultante.
constroi inst = auxiliarconstroi (instrucoes inst (editorInicial inst)) 
          
-- | Função que de um 'Editor' produz um 'Mapa' 
auxiliarconstroi::Editor -- ^ O 'Editor' resultande das 'Instrucoes'
                -> Mapa  -- ^ O 'Mapa' correspondente ao 'Editor'
auxiliarconstroi (Editor p d t w m) = m


