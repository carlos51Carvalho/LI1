-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2018li1g146 where

import LI11819
import Tarefa0_2018li1g146
import Tarefa1_2018li1g146

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [ (1, (Dispara Canhao), (Estado mapa0 [jogador0,jogador1] [])), 
             (1, (Dispara Canhao), (Estado mapa0 [jogador0,jogador1] [])), 
             (1, (Movimenta C), (Estado mapa0 [jogador0,jogador1] [])), 
             (0, (Movimenta C), (Estado mapa0 [jogador2,jogador1] [])), 
             (0, (Movimenta C), (Estado mapa0 [jogador0,jogador1] [DisparoChoque 1 5])),
             (0, (Dispara Laser), (Estado mapa0 [jogador0,jogador1] [])), 
             (1, (Movimenta C), (Estado mapa0 [jogador0,jogador3] [])), 
             (0, (Dispara Laser), (Estado mapa0 [jogador0,jogador1] [DisparoChoque 1 5])), 
             (0, (Dispara Laser), (Estado mapa0 [jogador0,jogador4] [DisparoChoque 1 5])), 
             (2, (Dispara Laser), (Estado mapa0 [jogador0,jogador4,jogador1] [DisparoChoque 1 5])), 
             (3, (Dispara Laser), (Estado mapa0 [jogador0,jogador4,jogador1,jogador5] [DisparoChoque 1 5])), 
             (0, (Dispara Canhao), (Estado mapa0 [jogador0,jogador1] []))]

-- | Auxiliares pre defenidos para criar mais facilmente testes
mapa0 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]



listaJog::[Jogador]
listaJog = [jogador0, jogador1, jogador2, jogador3]

jogador0 = (Jogador (2,5) B 1 1 1) 
jogador1 = (Jogador (3,1) C 3 0 2)
jogador2 = (Jogador (1,5) D 0 2 0)
jogador3 = (Jogador (5,5) C 3 1 1) 
jogador4 = (Jogador (4,5) C 3 1 1)
jogador5 = (Jogador (2,5) B 0 0 0) 


-- * Funções principais da Tarefa 2.

-- | Efetua uma jogada.
-- * 'Jogada' - Define os possiveis movimentos ou ações do tank, disparar ou mover
-- * 'Jogador' - Define um dos jogadores presente, recebe um par de Int's, e mais 4 Int's
-- * 'Disparo' - Os diferentes projeteis presentes no mapa que sao postos na jogada de um jogador e variam em 3, sendo Canhao, laser, choque, podem receber um par de Int um Int que identifica o jogador e uma Direcao, ou apenas o Int identificador e Ticks que o diparo ficara ativo

jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada.
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada n inst (Estado a j d) = (Estado a (chgJ j n inst a d) (alteracaoD n (pln j n) inst d))

-- | Dado uma lista de jogadores e o nomero de jogador dá o jogador desejado (player number)
pln :: [Jogador] -> Int -> Jogador
pln (b:bs) 0 = b
pln (b:bs) n = pln bs (n-1)

-- | Dado o jogador e a lista de jogadores diz qual e o Int que o identifica (number of player)
nofpl :: [Jogador] -> Int -> Jogador -> Int
nofpl [] cont (Jogador (l,c) d vid al ac) = cont
nofpl (b:bs) cont (Jogador (l,c) d vid al ac) | (Jogador (l,c) d vid al ac) == b = cont
nofpl (b:bs) cont (Jogador (l,c) d vid al ac) | (Jogador (l,c) d vid al ac) /= b = nofpl bs (cont+1) (Jogador (l,c) d vid al ac)

-- | Altera o Jogador na lista [Jogador] ja fornecida (change player)
chgJ :: [Jogador] -> Int -> Jogada -> Mapa -> [Disparo] -> [Jogador]
chgJ [] x inst a dps = []
chgJ (b:bs) 0 inst a dps = (alteracaoJ b inst a dps (b:bs)) : bs
chgJ (b:bs) n inst a dps = b : chgJ bs (n-1) inst a dps 


-- | Depois de dar um jogador e uma jogada altera o jogador que foi dado
alteracaoJ :: Jogador -> Jogada -> Mapa -> [Disparo] -> [Jogador] -> Jogador 
alteracaoJ (Jogador (l,c) d 0 al ac) _ _ _ _ = (Jogador(l,c) d 0 al ac)
alteracaoJ (Jogador (l,c) C vid al ac ) (Movimenta C) a dps pl |(encontraPosicaoMatriz (l-1,c) a == Vazia) && (encontraPosicaoMatriz (l-1,c+1) a == Vazia) && estouChocado dps (l,c) pl = (Jogador (l-1,c) C vid al ac )
                                                               |otherwise = (Jogador (l,c) C vid al ac )      
alteracaoJ (Jogador (l,c) B vid al ac ) (Movimenta B) a dps pl |(encontraPosicaoMatriz (l+2,c) a == Vazia) && (encontraPosicaoMatriz (l+2,c+1) a == Vazia) && estouChocado dps (l,c) pl  = (Jogador (l+1,c) B vid al ac )
                                                               |otherwise = (Jogador (l,c) B vid al ac )
alteracaoJ (Jogador (l,c) D vid al ac ) (Movimenta D) a dps pl |(encontraPosicaoMatriz (l,c+2) a == Vazia) && (encontraPosicaoMatriz (l+1,c+2) a == Vazia) && estouChocado dps (l,c) pl = (Jogador (l,c+1) D vid al ac )
                                                               |otherwise = (Jogador (l,c) D vid al ac )
alteracaoJ (Jogador (l,c) E vid al ac ) (Movimenta E) a dps pl |(encontraPosicaoMatriz (l,c-1) a == Vazia) && (encontraPosicaoMatriz (l+1,c-1) a == Vazia) && estouChocado dps (l,c) pl = (Jogador (l,c-1) E vid al ac )
                                                               |otherwise = (Jogador (l,c) E vid al ac )

alteracaoJ (Jogador (l,c) d vid al ac ) (Movimenta C) a dps pl | d /= C = (Jogador(l,c) C vid al ac )  -- TESTAR SE A JOGADA QUA(Jogada nd)O MA(Jogada nd)A UM Jogador  NUMA DIREÇÃO DIFERENTE DA QUE ELE ESTA A SER APONTADO SE ELE SE MOVE
alteracaoJ (Jogador (l,c) d vid al ac ) (Movimenta B) a dps pl | d /= B = (Jogador(l,c) B vid al ac )
alteracaoJ (Jogador (l,c) d vid al ac ) (Movimenta D) a dps pl | d /= D = (Jogador(l,c) D vid al ac )
alteracaoJ (Jogador (l,c) d vid al ac ) (Movimenta E) a dps pl | d /= E = (Jogador(l,c) E vid al ac )

alteracaoJ (Jogador (l,c) d vid 0 ac) (Dispara Laser) a dps pl = (Jogador (l,c) d vid 0 ac)
alteracaoJ (Jogador (l,c) d vid al ac) (Dispara Laser) a dps pl = (Jogador (l,c) d vid (al-1) ac)
alteracaoJ (Jogador (l,c) d vid al 0) (Dispara Choque) a dps pl = (Jogador (l,c) d vid al 0)
alteracaoJ (Jogador (l,c) d vid al ac) (Dispara Choque) a dps pl = (Jogador (l,c) d vid al (ac-1))
alteracaoJ (Jogador (l,c) d vid al ac) (Dispara Canhao) a dps pl = (Jogador (l,c) d vid al ac)


-- | Dependendo do jogador e da Jogada altera o lista de Disparo's presente
alteracaoD :: Int -> Jogador -> Jogada -> [Disparo] -> [Disparo]
alteracaoD n (Jogador(l,c) d 0 al ac) _ b = b
alteracaoD n (Jogador(l,c) d vid al ac ) (Movimenta nd) b | nd == C || nd == D || nd == E || nd == B = b
alteracaoD n (Jogador(l,c) d vid 0 ac) (Dispara Laser) b = b
alteracaoD n (Jogador(l,c) C vid al ac) (Dispara Laser) b = b ++ [(DisparoLaser n (l-1,c) C)]
alteracaoD n (Jogador(l,c) B vid al ac) (Dispara Laser) b = b ++ [(DisparoLaser n (l+1,c) B)]
alteracaoD n (Jogador(l,c) D vid al ac) (Dispara Laser) b = b ++ [(DisparoLaser n (l,c+1) D)]
alteracaoD n (Jogador(l,c) E vid al ac) (Dispara Laser) b = b ++ [(DisparoLaser n (l,c-1) E)]
alteracaoD n (Jogador(l,c) C vid al ac) (Dispara Canhao) b = b ++ [(DisparoCanhao n (l-1,c) C)]
alteracaoD n (Jogador(l,c) B vid al ac) (Dispara Canhao) b = b ++ [(DisparoCanhao n (l+1,c) B)]
alteracaoD n (Jogador(l,c) D vid al ac) (Dispara Canhao) b = b ++ [(DisparoCanhao n (l,c+1) D)]
alteracaoD n (Jogador(l,c) E vid al ac) (Dispara Canhao) b = b ++ [(DisparoCanhao n (l,c-1) E)]
alteracaoD n (Jogador(l,c) d vid al 0) (Dispara Choque) b = b
alteracaoD n (Jogador(l,c) d vid al ac) (Dispara Choque) b = b ++ [(DisparoChoque n 5)]

-- | Verifica se um Jogador esta abrangido pelo disparo choque e se está ou não sob o efeito de um determinado choque 
estouChocado :: [Disparo] -- ^  Recebe uma lista de disparos 
             -> PosicaoGrelha -- ^  A PosicaoGrelha do Jogador que se quer mexer 
             -> [Jogador] -- ^  A lista de Jogadores
             -> Bool -- ^  Retorna um Bool 
estouChocado _ _ _ = True
estouChocado ((DisparoChoque 0 tic):t) (a,b) ((Jogador (x,y) d v l c):t2)          | eMeu (a,b) (x,y) = estouChocado t (a,b) ((Jogador (x,y) d v l c):t2)
                                                                                   | otherwise =  elem (a,b) (areaDisparo (x,y))
estouChocado ((DisparoChoque 1 tic):t) (a,b) (x1:(Jogador (x,y) d v l c):t2)       | eMeu (a,b) (x,y) = estouChocado t (a,b) (x1:(Jogador (x,y) d v l c):t2) 
                                                                                   | otherwise = elem (a,b) (areaDisparo (x,y))
estouChocado ((DisparoChoque 2 tic):t) (a,b) (x1:y2:(Jogador (x,y) d v l c):t2)    | eMeu (a,b) (x,y) = estouChocado t (a,b) (x1:y2:(Jogador (x,y) d v l c):t2)
                                                                                   | otherwise = elem (a,b) (areaDisparo (x,y))
estouChocado ((DisparoChoque 3 tic):t) (a,b) (x1:y2:z3:(Jogador (x,y) d v l c):[]) | eMeu (a,b) (x,y)  = estouChocado t (a,b) (x1:y2:z3:(Jogador (x,y) d v l c):[]) 
                                                                                   | otherwise = elem (a,b) (areaDisparo (x,y))




-- | Verifica se Jogador  a mexer e o a ser testado na lista são o mesmo .
eMeu :: PosicaoGrelha -- ^  PosicaoGrelha do Jogador a mexertetrominoPara
     -> PosicaoGrelha -- ^  PosicaoGrelha do Jogador a testar na lista de Jogadores
     -> Bool          -- ^  Caso o Jogador a mexer e o a testar forem o mesmo retorna True
eMeu (a,b) (x,y) = a == x && b == y

-- | Verifica todas as Posicao dentro da area de choque
areaDisparo :: PosicaoGrelha -- ^  Posicao do Jogador que dispara
            -> [PosicaoGrelha] -- ^ Lista de todas as Posicoes que o choque abrange
areaDisparo (a,b) = [(a-3,b-3),(a-3,b-2),(a-3,b-1),(a-3,b),(a-3,b+1),(a-3,b+2),(a-3,b+3),(a-2,b-3),(a-2,b-2),(a-2,b-1),(a-2,b),(a-2,b+1),(a-2,b+2),(a-2,b+3),(a-1,b-3),(a-1,b-2),(a-1,b-1),(a-1,b),(a-1,b+1),(a-1,b+2),(a-1,b+3),(a,b-3),(a,b-2),(a,b-1),(a,b),(a,b+1),(a,b+2),(a,b+3),(a+1,b-3),(a+1,b-2),(a+1,b-1),(a+1,b),(a+1,b+1),(a+1,b+2),(a+1,b+3),(a+2,b-3),(a+2,b-2),(a+2,b-1),(a+2,b),(a+2,b+1),(a+2,b+2),(a+2,b+3),(a+3,b-3),(a+3,b-2),(a+3,b-1),(a+3,b),(a+3,b+1),(a+3,b+2),(a+3,b+3)]










{-

hajogador :: [Jogador] -> Jogador -> Bool
hajogador [] j = False
hajogador ((Jogador (x,y) d v l c):t) (Jogador (n,b) di vi li ci) | (Jogador (x,y) d v l c) == (Jogador (n,b) di vi li ci) = hajogador t (Jogador (n,b) di vi li ci) 
                                                                  | x+1 == n && y-1 || x+1 == n && y-1 || x+1 == n && y-1 || x+1 == n && y-1 == b = False
                                                                  | otherwise = True





-- | Verifica se uma posicao  esta presente num disparo do choque, True o movimento e permitido, False e negado
--moveChoque :: (Int,Int) -> [Disparo] -> [Jogador] -> Int  -> Bool
--moveChoque (l,c) ((DisparoCanhao n (ld,cd) dd):bs) a np  = True && moveChoque (l,c) bs a np
--moveChoque (l,c) ((DisparoLaser n (ld,cd) dd):bs) a np = True && moveChoque (l,c) bs a np
--moveChoque (l,c) ((DisparoChoque n t):bs) a np |np == n = True && moveChoque (l,c) bs a np
--                                               |np /= n && (l <= ((centrochoquel n a) + 3) && l >= ((centrochoquel n a) - 3)) && (c <= ((centrochoquec n a) + 3) && c >= ((centrochoquec n a) - 3)) = False 
--                                               |otherwise = True && moveChoque (l,c) bs a np
--moveChoque _ _ _ _ = True

-- | auxiliar da moveChoque que da o centro das linhas da matriz do choque
--centrochoquel :: Int -> [Jogador] -> Int
--centrochoquel 0 ((Jogador(l,c) d vid al ac):bs) = l
--centrochoquel n ((Jogador(l,c) d vid al ac):bs) = centrochoquel (n-1) bs

-- | auxiliar da moveChoque que da o centro das colunas da matriz do choque
--centrochoquec :: Int -> [Jogador] -> Int
--centrochoquec 0 ((Jogador(l,c) d vid al ac):bs) = c
--centrochoquec n ((Jogador(l,c) d vid al ac):bs) = centrochoquec (n-1) bs






verificaPl :: [Jogador] -> (Int,Int) -> Jogada -> Int -> Int -> Bool
verificaPl [] (_,_) _ _ _ = True
verificaPl ((Jogador(l,c) d vid al ac):bs) (m,n) inst cont np | cont == np = True && verificaPl bs (m,n) inst (cont + 1) np
verificaPl ((Jogador(l,c) d vid al ac):bs) (m,n) (Movimenta C) cont np | m == (l+2) && (n == c || n == (c+1) || n == (c-1)) = False
                                                                       | otherwise = True && verificaPl bs (m,n) (Movimenta C) (cont + 1) np
verificaPl ((Jogador(l,c) d vid al ac):bs) (m,n) (Movimenta B) cont np | m == (l-2) && (n == c || n == (c+1) || n == (c-1)) = False
                                                                       | otherwise = True && verificaPl bs (m,n) (Movimenta B) (cont + 1) np
verificaPl ((Jogador(l,c) d vid al ac):bs) (m,n) (Movimenta D) cont np | n == (c-2) && (m== l || m == (l+1) || m == (l-1)) = False
                                                                       | otherwise = True && verificaPl bs (m,n) (Movimenta D) (cont + 1) np
verificaPl ((Jogador(l,c) d vid al ac):bs) (m,n) (Movimenta E) cont np | n == (c+2) && (m== l || m == (l+1) || m == (l-1)) = False
                                                                       | otherwise = True && verificaPl bs (m,n) (Movimenta E) (cont + 1) np
-}