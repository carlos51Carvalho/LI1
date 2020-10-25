-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2018li1g146 where

import LI11819
import Tarefa0_2018li1g146

-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um 'Estado'.
testesT4 :: [Estado]
testesT4 = [e0, e1, e2, e3, e4, e5, e6, e7,e9]

e0 = (Estado mapa_1 [jogador6,jogador7] disparo0 )
e1 = (Estado mapa_1 [jogador6,jogador7] disparo1 )
e2 = (Estado mapa_1 [jogador6,jogador7] disparo2 )
e3 = (Estado mapa_1 [jogador6,jogador7] disparo3 )
e4 = (Estado mapa_1 [jogador6,jogador7] disparo4 )
e5 = (Estado mapa_2 [jogador8] disparo5 )
e6 = (Estado mapa_2 [jogador8] disparo6 )
e7 = (Estado mapa_1 [jogador6,jogador7] disparo7 )
e9 = (Estado mapa_1 [jogador7] disparo11)

mapa_1 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]
mapa_2 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]

jogador6 = (Jogador (2,2) B 1 1 1) 
jogador7 = (Jogador (5,5) C 3 0 2)
jogador8 = (Jogador (4,4) B 3 3 3)

disparo11 = [(DisparoLaser 0 (4,4) C)]
disparo0 = [(DisparoCanhao 0 (4,4) D), (DisparoCanhao 1 (5,4) C)]
disparo1 = [(DisparoCanhao 0 (4,4) D), (DisparoLaser 0 (4,2) D)]
disparo2 = [(DisparoCanhao 0 (4,4) D), (DisparoCanhao 1 (4,6) E)]
disparo3 = [(DisparoChoque 0 5)]
disparo4 = [(DisparoChoque 0 1)]
disparo5 = [(DisparoCanhao 0 (3,3) E)]
disparo6 = [(DisparoLaser 0 (3,5) E)]
disparo7 = [(DisparoCanhao 0 (1,2) E), (DisparoLaser 1 (3,2) E)]


-- * Funções principais da Tarefa 4.

-- | Avança o 'Estado' do jogo um 'Tick' de tempo.
--
-- __NB:__ Apenas os 'Disparo's afetam o 'Estado' do jogo com o passar do tempo.
--
-- __NB:__ Deve chamar as funções 'tickChoques', 'tickCanhoes' e 'tickLasers' pela ordem definida.
tick :: Estado -- ^ O 'Estado' anterior.
     -> Estado -- ^ O 'Estado' após um 'Tick'.
tick = tickChoques . tickCanhoes . tickLasers

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos tiros de 'Laser' disparados.
tickLasers :: Estado -> Estado
tickLasers (Estado a pl dps) = (Estado (mapaLaser dps a) (jogadorLaser dps pl a) (finalLaser dps))


-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos das balas de 'Canhao' disparadas.
tickCanhoes :: Estado -> Estado
tickCanhoes (Estado a pl dps) = (Estado (mapaCanhao dps a pl) (jogadorCanhao dps pl a) (finalCanhao dps))

-- | Avança o 'Estado' do jogo um 'Tick' de tempo, considerando apenas os efeitos dos campos de 'Choque' disparados.
tickChoques :: Estado -> Estado
tickChoques (Estado a pl dps) = Estado a pl (upDpsChoque(dps))


-- Jogador

jogadorLaser :: [Disparo] -> [Jogador] -> Mapa -> [Jogador]
jogadorLaser [] b a = b
jogadorLaser _ [] a = []
jogadorLaser (b:bs) pl a = danoTotal pl (movimentaLaser b a bs)


jogadorCanhao :: [Disparo] -> [Jogador] -> Mapa -> [Jogador]
jogadorCanhao [] j a = j
jogadorCanhao _ [] a = []
jogadorCanhao (b:bs) pl a = danoTotal pl (movimentaCanhao b a bs pl)



-- * Mapa funcoes que alteram o mapa dependendo do disparo e do mapa


updateMapa :: Disparo -> Mapa -> Mapa  
updateMapa dps [] = []
updateMapa (DisparoLaser n (l,c) C) a | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l,c+1) a) /= Bloco Destrutivel = atualizaPosicaoMatriz (l,c) Vazia a
                                      | (encontraPosicaoMatriz (l,c) a) /= Bloco Destrutivel && (encontraPosicaoMatriz (l,c+1) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l,c+1) Vazia a
                                      | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l,c+1) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l,c+1) Vazia (atualizaPosicaoMatriz (l,c) Vazia a)
                                      |otherwise = a
updateMapa (DisparoLaser n (l,c) B) a | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l,c+1) a) /= Bloco Destrutivel = atualizaPosicaoMatriz (l,c) Vazia a
                                      | (encontraPosicaoMatriz (l,c) a) /= Bloco Destrutivel && (encontraPosicaoMatriz (l,c+1) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l,c+1) Vazia a
                                      | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l,c+1) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l,c+1) Vazia (atualizaPosicaoMatriz (l,c) Vazia a)
                                      |otherwise = a 
updateMapa (DisparoLaser n (l,c) D) a | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l-1,c) a) /= Bloco Destrutivel = atualizaPosicaoMatriz (l,c) Vazia a
                                      | (encontraPosicaoMatriz (l,c) a) /= Bloco Destrutivel && (encontraPosicaoMatriz (l-1,c) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l-1,c) Vazia a
                                      | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l-1,c) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l-1,c) Vazia (atualizaPosicaoMatriz (l,c) Vazia a)
                                      |otherwise = a 
updateMapa (DisparoLaser n (l,c) E) a | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l-1,c) a) /= Bloco Destrutivel = atualizaPosicaoMatriz (l,c) Vazia a
                                      | (encontraPosicaoMatriz (l,c) a) /= Bloco Destrutivel && (encontraPosicaoMatriz (l-1,c) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l-1,c) Vazia a
                                      | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l-1,c) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l-1,c) Vazia (atualizaPosicaoMatriz (l,c) Vazia a)
                                      |otherwise = a 
updateMapa (DisparoCanhao n (l,c) C) a | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l,c+1) a) /= Bloco Destrutivel = atualizaPosicaoMatriz (l,c) Vazia a
                                       | (encontraPosicaoMatriz (l,c) a) /= Bloco Destrutivel && (encontraPosicaoMatriz (l,c+1) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l,c+1) Vazia a
                                       | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l,c+1) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l,c+1) Vazia (atualizaPosicaoMatriz (l,c) Vazia a)
                                       |otherwise = a
updateMapa (DisparoCanhao n (l,c) B) a | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l,c+1) a) /= Bloco Destrutivel = atualizaPosicaoMatriz (l,c) Vazia a
                                       | (encontraPosicaoMatriz (l,c) a) /= Bloco Destrutivel && (encontraPosicaoMatriz (l,c+1) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l,c+1) Vazia a
                                       | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l,c+1) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l,c+1) Vazia (atualizaPosicaoMatriz (l,c) Vazia a)
                                       |otherwise = a 
updateMapa (DisparoCanhao n (l,c) D) a | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l-1,c) a) /= Bloco Destrutivel = atualizaPosicaoMatriz (l,c) Vazia a
                                       | (encontraPosicaoMatriz (l,c) a) /= Bloco Destrutivel && (encontraPosicaoMatriz (l-1,c) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l-1,c) Vazia a
                                       | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l-1,c) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l-1,c) Vazia (atualizaPosicaoMatriz (l,c) Vazia a)
                                       |otherwise = a 
updateMapa (DisparoCanhao n (l,c) E) a | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l-1,c) a) /= Bloco Destrutivel = atualizaPosicaoMatriz (l,c) Vazia a
                                       | (encontraPosicaoMatriz (l,c) a) /= Bloco Destrutivel && (encontraPosicaoMatriz (l-1,c) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l-1,c) Vazia a
                                       | (encontraPosicaoMatriz (l,c) a) == Bloco Destrutivel && (encontraPosicaoMatriz (l-1,c) a) == Bloco Destrutivel = atualizaPosicaoMatriz (l-1,c) Vazia (atualizaPosicaoMatriz (l,c) Vazia a)
                                       |otherwise = a
updateMapa (DisparoChoque n nd) a = a

updateMapaTotal :: [Disparo] -> Mapa -> Mapa
updateMapaTotal [] a = a
updateMapaTotal d a = updateMapa (head d) a

mapaLaser :: [Disparo] -> Mapa -> Mapa
mapaLaser [] a = a
mapaLaser (b:bs) a = updateMapaTotal (movimentaLaser b a bs) a

mapaCanhao :: [Disparo] -> Mapa -> [Jogador] -> Mapa
mapaCanhao _ a [] = a 
mapaCanhao [] a _ = a
mapaCanhao (b:bs) a pl = updateMapaTotal (movimentaCanhao b a bs pl) a


-- * Disparo funcoes que alteram os disparos

disparoHit :: Jogador -> Disparo -> Bool 
disparoHit (Jogador (l,c) d vid al ac) (DisparoLaser n (ld,cd) nd) |( l == ld || l == (ld+1)) && (c == cd || c == (cd+1)) = True
                                                                   |otherwise = False
disparoHit (Jogador (l,c) d vid al ac) (DisparoCanhao n (ld,cd) nd) | (l == ld || l == (ld+1)) && (c == cd || c == (cd+1)) = True
                                                                    |otherwise = False

danoRonda :: [Jogador] -> Disparo -> [Jogador]
danoRonda [] _ = []
danoRonda ((Jogador (l,c) d vid al ac):pl) (DisparoLaser n (ld,cd) nd) | disparoHit (Jogador (l,c) d vid al ac) (DisparoLaser n (ld,cd) nd)  = (Jogador (l,c) d (vid-1) al ac) : (danoRonda pl (DisparoLaser n (ld,cd) nd))
                                                                       | otherwise = (Jogador (l,c) d vid al ac) : (danoRonda pl (DisparoLaser n (ld,cd) nd))
danoRonda ((Jogador (l,c) d vid al ac):pl) (DisparoCanhao n (ld,cd) nd) | disparoHit (Jogador (l,c) d vid al ac) (DisparoCanhao n (ld,cd) nd) = ((Jogador (l,c) d (vid-1) al ac):pl) 
                                                                        | otherwise = (Jogador (l,c) d vid al ac) : (danoRonda pl (DisparoCanhao n (ld,cd) nd))
danoRonda pl (DisparoChoque n nd) = pl 


danoTotal :: [Jogador] -> [Disparo] -> [Jogador]
danoTotal a [] = a
danoTotal a d = danoRonda a (head d)

removeDisparos :: [Disparo] -> Disparo -> [Disparo]
removeDisparos [] dps = [] 
removeDisparos [x] dps = []
removeDisparos (b:bs) dps = if b == dps then bs
                      else b: removeDisparos bs dps

movimentaLaser :: Disparo -> Mapa -> [Disparo] -> [Disparo]
movimentaLaser (DisparoLaser n (l,c) nd) [] dps = (DisparoLaser n (l,c) nd) : dps
movimentaLaser (DisparoLaser n (l,c) C ) a dps | (encontraPosicaoMatriz (l-1,c) a) /= Bloco Indestrutivel && (encontraPosicaoMatriz (l-1,c+1) a) /= Bloco Indestrutivel = movimentaLaser (DisparoLaser n (l-1,c) C) a ((DisparoLaser n (l-1,c) C): (removeDisparos dps (DisparoLaser n (l,c) C)) )
                                              |otherwise = removeDisparos dps (DisparoLaser n (l,c) C)
movimentaLaser (DisparoLaser n (l,c) B ) a dps | (encontraPosicaoMatriz (l+1,c) a) /= Bloco Indestrutivel && (encontraPosicaoMatriz (l+1,c+1) a) /= Bloco Indestrutivel = movimentaLaser (DisparoLaser n (l+1,c) B) a ((DisparoLaser n (l+1,c) B): (removeDisparos dps (DisparoLaser n (l,c) B)) ) 
                                              |otherwise = removeDisparos dps (DisparoLaser n (l,c) B)
movimentaLaser (DisparoLaser n (l,c) D ) a dps | (encontraPosicaoMatriz (l,c+1) a) /= Bloco Indestrutivel && (encontraPosicaoMatriz (l-1,c+1) a) /= Bloco Indestrutivel = movimentaLaser (DisparoLaser n (l,c+1) D) a ((DisparoLaser n (l,c+1) D): (removeDisparos dps (DisparoLaser n (l,c) D)) )
                                              |otherwise = removeDisparos dps (DisparoLaser n (l,c) D)
movimentaLaser (DisparoLaser n (l,c) E ) a dps | (encontraPosicaoMatriz (l,c-1) a) /= Bloco Indestrutivel && (encontraPosicaoMatriz (l-1,c-1) a) /= Bloco Indestrutivel = movimentaLaser (DisparoLaser n (l,c-1) E) a ((DisparoLaser n (l,c-1) E): (removeDisparos dps (DisparoLaser n (l,c) E)) )
                                              |otherwise = removeDisparos dps (DisparoLaser n (l,c) E)
movimentaLaser d _ dps = dps

colisaoLaser :: Disparo -> Disparo -> Bool
colisaoLaser (DisparoLaser n (l,c) nd) (DisparoLaser pl (ld,cd) d) = False
colisaoLaser (DisparoLaser n (l,c) nd) (DisparoChoque pl t) = False
colisaoLaser (DisparoLaser n (l,c) nd) (DisparoCanhao pl (ld,cd) d) | d == C || d == B && (l == ld || c == cd || c == (cd+1)) = True
                                                            | d == C || d == B && (l == ld || l == (ld+1) || l == (ld-1) && c == cd) = True
                                                            |otherwise = False 
colisaoLaser _ _ = False

colisaoLista :: Disparo -> [Disparo] -> [Disparo]
colisaoLista a [] = []
colisaoLista dps (b:bs) | dps == b = b : colisaoLista dps bs
                        | colisaoLaser dps b = colisaoLista dps bs
                        | otherwise = b : (colisaoLista dps bs)

finalLaser :: [Disparo] -> [Disparo]
finalLaser [] = []
finalLaser (b:dps) = colisaoLista b dps 

movimentaCanhao :: Disparo -> Mapa -> [Disparo] -> [Jogador] -> [Disparo] --deixar lista e fazer correr cada execuçao com lista de update
movimentaCanhao (DisparoCanhao n (l,c) nd) [] dps pl = (DisparoCanhao n (l,c) nd) : dps
movimentaCanhao (DisparoCanhao n (l,c) C) a dps pl | (encontraPosicaoMatriz (l-1,c) a) == Vazia && (encontraPosicaoMatriz (l-1,c+1) a) == Vazia &&  colisaoListaC (DisparoCanhao n (l,c) C) dps == False && colisaoCanhaoJogador (DisparoCanhao n (l,c) C) pl == False = ((DisparoCanhao n (l-1,c) C):(removeDisparos dps (DisparoCanhao n (l,c) C)) )
                                                   |otherwise = removeDisparos dps (DisparoCanhao n (l,c) C)
movimentaCanhao (DisparoCanhao n (l,c) B) a dps pl | (encontraPosicaoMatriz (l+1,c) a) == Vazia && (encontraPosicaoMatriz (l-1,c+1) a) == Vazia &&  colisaoListaC (DisparoCanhao n (l,c) B) dps == False && colisaoCanhaoJogador (DisparoCanhao n (l,c) B) pl == False = ((DisparoCanhao n (l+1,c) B):(removeDisparos dps (DisparoCanhao n (l,c) B)) ) 
                                                   |otherwise = removeDisparos dps (DisparoCanhao n (l,c) B)
movimentaCanhao (DisparoCanhao n (l,c) D) a dps pl | (encontraPosicaoMatriz (l,c+1) a) == Vazia && (encontraPosicaoMatriz (l-1,c+1) a) == Vazia &&  colisaoListaC (DisparoCanhao n (l,c) D) dps == False && colisaoCanhaoJogador (DisparoCanhao n (l,c) D) pl == False = ((DisparoCanhao n (l,c+1) D):(removeDisparos dps (DisparoCanhao n (l,c) D)) )
                                                   |otherwise = removeDisparos dps (DisparoCanhao n (l,c) D)
movimentaCanhao (DisparoCanhao n (l,c) E) a dps pl | (encontraPosicaoMatriz (l,c-1) a) == Vazia && (encontraPosicaoMatriz (l-1,c-1) a) == Vazia &&  colisaoListaC (DisparoCanhao n (l,c) E) dps == False && colisaoCanhaoJogador (DisparoCanhao n (l,c) E) pl == False = ((DisparoCanhao n (l,c-1) E):(removeDisparos dps (DisparoCanhao n (l,c) E)) )
                                                   |otherwise = removeDisparos dps (DisparoCanhao n (l,c) E)
movimentaCanhao _ a dps pl = dps

colisaoCanhao :: Disparo -> Disparo -> Bool
colisaoCanhao (DisparoCanhao n (l,c) nd) (DisparoChoque pl t) = False
colisaoCanhao (DisparoCanhao n (l,c) nd) (DisparoLaser pl (ld,cd) d) | d == C || d == B && (l == ld || c == cd || c == (cd+1)) = True
                                                                     | d == C || d == B && (l == ld || l == (ld+1) || c == cd) = True
                                                                     |otherwise = False 
colisaoCanhao (DisparoCanhao n (l,c) nd) (DisparoCanhao pl (ld,cd) d) | d == C || d == B && (l == ld || c == cd || c == (cd+1)) = True
                                                                      | d == C || d == B && (l == ld || l == (ld+1) || c == cd) = True
                                                                      |otherwise = False

colisaoCanhaoJogador :: Disparo -> [Jogador] -> Bool
colisaoCanhaoJogador _ [] = False
colisaoCanhaoJogador (DisparoCanhao n (l,c) d) (b:bs) | disparoHit b (DisparoCanhao n (l,c) d) = True && colisaoCanhaoJogador (DisparoCanhao n (l,c) d) bs
                                                      |otherwise = False

colisaoListaC :: Disparo -> [Disparo] -> Bool
colisaoListaC a [] = False
colisaoListaC dps (b:bs) | dps == b = False
                         | colisaoCanhao dps b = True
                         | otherwise = False

finalCanhao :: [Disparo] -> [Disparo]
finalCanhao [] = []
finalCanhao (b:dps) = colisaoLista b dps 



upDpsChoque :: [Disparo] -> [Disparo]
upDpsChoque [] = [] 
upDpsChoque ((DisparoChoque n t):dps) | t == 0 = upDpsChoque dps
                                      |otherwise = (DisparoChoque n (t-1)) : upDpsChoque dps
upDpsChoque (b:bs) = b : upDpsChoque bs
