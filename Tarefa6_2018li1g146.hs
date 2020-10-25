{- | Module      : Tarefa6_2018li1g146
     Description : Compressão e descompressao do estado de um jogo (mapa, lista de Jogadores e lista de disparos)
     Copyright   : Carlos Carvalho <a89605@alunos.uminho.pt>
                   Fernando Lopes <a89472@alunos.uminho.pt> 

= Resumo:
Nesta tarefa o objetivo é a construção de um bot que para além de outros jogadores consiga apresentar alguma competição no jogo. Este pode jogar contra não só contra jogador mas 
também contra outros bots. Para a construção do bot decidimos focar três aspetos que consideramos mais importantes, sendo a primeiro posição de disparos perto do jogador(bot), a
posição dos outros jogador e a capacidade de disparar para eles. 

= Introdução :
Tentamos trabalhar o bot para que este seja mais agressivo porém tentamos dar prioridade a este conseguir desviar de disparos perigosos ou que estejam a andar na direção do bot
caso estes ja não existam ou nenhum tenha sido disparado o bot vai tentar aproximar dos outros jogadores enquanto dispara quando estes se aproximem da mira do bot.

= Objetivos e estratégias:

== Agressividade: 
O bot vai tentar progressivamente se aproximar do inimigo para poder fazer disparos que por causa do pequeno tempo de reação tem maior prioridade de conectar com o alvo escolhido
para além disso quando este encontrar uma boa situação vai tentar usar os lazers para reduzir o numero de vidas de um dos jogadores em causa e assim não so ganha pontos como reduz
as possibilidades de outros ganharem pontos.

== Proteção própria: 
Apesar da natureza agressiva do bot a sua primeira prioridade é tentar deviar de disparos que se aproximem bastante dele e caso não seja possivel este fugir desses disparos este tenta
disparar contra eles como ultima opção para conseguir evitar dano.

= Conclusão:
No final o bot tem o objetivo de marioritariamente matar os inimigos porque assim reduz a capacidade que estes tem de fazer pontos e assim acrescentado pontos para si mesmo.
     -}

-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2018li1g146 where

import LI11819
import Tarefa0_2018li1g146

-- * Funções principais da Tarefa 6.

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot a (Estado m pl dps) | disparoIminenteC (dps) (jogadorBot pl a) && encontraPosicaoMatriz (posicaoE (jogadorBot pl a)) (m)  == Vazia && direcaoBot (jogadorBot pl a) == E = Just (Movimenta E) 
                        | disparoIminenteC (dps) (jogadorBot pl a) && encontraPosicaoMatriz (posicaoD (jogadorBot pl a)) (m) == Vazia && direcaoBot (jogadorBot pl a) == D = Just (Movimenta D)
                        | disparoIminenteC (dps) (jogadorBot pl a) && direcaoBot (jogadorBot pl a) == C = Just (Dispara Canhao)
                        | disparoIminenteB (dps) (jogadorBot pl a) && encontraPosicaoMatriz (posicaoE (jogadorBot pl a)) (m)  == Vazia && direcaoBot (jogadorBot pl a) == E = Just (Movimenta E) 
                        | disparoIminenteB (dps) (jogadorBot pl a) && encontraPosicaoMatriz (posicaoD (jogadorBot pl a)) (m) == Vazia && direcaoBot (jogadorBot pl a) == D = Just (Movimenta D)
                        | disparoIminenteB (dps) (jogadorBot pl a) && direcaoBot (jogadorBot pl a) == B = Just (Dispara Canhao)
                        | disparoIminenteD (dps) (jogadorBot pl a) && encontraPosicaoMatriz (posicaoC (jogadorBot pl a)) (m)  == Vazia && direcaoBot (jogadorBot pl a) == C = Just (Movimenta E) 
                        | disparoIminenteD (dps) (jogadorBot pl a) && encontraPosicaoMatriz (posicaoB (jogadorBot pl a)) (m) == Vazia && direcaoBot (jogadorBot pl a) == B = Just (Movimenta D)
                        | disparoIminenteD (dps) (jogadorBot pl a) && direcaoBot (jogadorBot pl a) == D = Just (Dispara Canhao)
                        | disparoIminenteE (dps) (jogadorBot pl a) && encontraPosicaoMatriz (posicaoC (jogadorBot pl a)) (m)  == Vazia && direcaoBot (jogadorBot pl a) == C = Just (Movimenta E) 
                        | disparoIminenteE (dps) (jogadorBot pl a) && encontraPosicaoMatriz (posicaoB (jogadorBot pl a)) (m) == Vazia && direcaoBot (jogadorBot pl a) == B = Just (Movimenta D)
                        | disparoIminenteE (dps) (jogadorBot pl a) && direcaoBot (jogadorBot pl a) == E = Just (Dispara Canhao)
                        | disparoperigoso (dps) (jogadorBot pl a) pl && encontraPosicaoMatriz (posicaoE (jogadorBot pl a)) (m)  == Vazia = Just (Movimenta E)
                        | disparoperigoso (dps) (jogadorBot pl a) pl && encontraPosicaoMatriz (posicaoD (jogadorBot pl a)) (m)  == Vazia = Just (Movimenta D)
                        | disparoperigoso (dps) (jogadorBot pl a) pl && encontraPosicaoMatriz (posicaoC (jogadorBot pl a)) (m)  == Vazia = Just (Movimenta C)
                        | disparoperigoso (dps) (jogadorBot pl a) pl && encontraPosicaoMatriz (posicaoB (jogadorBot pl a)) (m)  == Vazia = Just (Movimenta B)
                        | existeJogadorLinha (pl) (jogadorBot pl a) && temlaser (jogadorBot pl a)  = Just (Dispara Laser)
                        | existeJogadorColuna (pl) (jogadorBot pl a) && temlaser (jogadorBot pl a) = Just (Dispara Laser)
                        | jogadorProximo (pl) (jogadorBot pl a) && temChoque (jogadorBot pl a) = Just (Dispara Choque)
                        | existeJogadorLinha (pl) (jogadorBot pl a) && temlaser (jogadorBot pl a) == False = Just (Dispara Canhao)
                        | existeJogadorColuna (pl) (jogadorBot pl a) && temlaser (jogadorBot pl a) == False = Just (Dispara Canhao)
                        | jogadorProximoC (pl) (jogadorBot pl a) && encontraPosicaoMatriz (posicaoC (jogadorBot pl a)) (m) == Vazia = Just (Movimenta C)
                        | jogadorProximoB (pl) (jogadorBot pl a) && encontraPosicaoMatriz (posicaoB (jogadorBot pl a)) (m) == Vazia = Just (Movimenta B)
                        | jogadorProximoD (pl) (jogadorBot pl a) && encontraPosicaoMatriz (posicaoD (jogadorBot pl a)) (m) == Vazia = Just (Movimenta D)
                        | jogadorProximoE (pl) (jogadorBot pl a) && encontraPosicaoMatriz (posicaoE (jogadorBot pl a)) (m) == Vazia = Just (Movimenta E)
                        | existeJogadorLinha (pl) (jogadorBot pl a) && estaVoltadoLinha (pl) (jogadorBot pl a) && encontraPosicaoMatriz (posicaoC (jogadorBot pl a)) (m) == Vazia && direcaoBot (jogadorBot pl a) == C = Just (Movimenta C)
                        | existeJogadorLinha (pl) (jogadorBot pl a) && estaVoltadoLinha (pl) (jogadorBot pl a) && encontraPosicaoMatriz (posicaoB (jogadorBot pl a)) (m) == Vazia && direcaoBot (jogadorBot pl a) == B = Just (Movimenta B)
                        | otherwise = Just (Movimenta D)

disparoperigoso :: [Disparo] -> Jogador -> [Jogador] -> Bool
disparoperigoso [] pl pls = False
disparoperigoso ((DisparoLaser n (c,l) d):bs) pl pls = False || disparoperigoso bs pl pls
disparoperigoso ((DisparoChoque n t):bs) pl pls | perigoChoque (jogadorBot pls n) (pl) = True
                                                | otherwise = False || disparoperigoso bs pl pls
disparoperigoso ((DisparoCanhao n (c,l) d):bs) (Jogador (x,y) vid v cl ch) pls | (c <= (x+3) && l <= (y+3) && l >= (y-3) && d == C) || (c >= (x-3) && l <= (y+3) && l >= (y-3) && d == B) || (l <= (y+3) && c <= (x+3) && c >= (x-3) && d == D) || (l >= (y-3) && c <= (x+3) && c >= (x-3) && d == E) = True
                                                                               | otherwise = False || disparoperigoso bs (Jogador (x,y) vid v cl ch) pls

perigoChoque :: Jogador -> Jogador -> Bool
perigoChoque (Jogador (c,l) d v cl ch) (Jogador (x,y) di vi g h) | (c <= (x+5) && c >= (x-5)) && (l <= (y+5) && l >= (y-5)) = True
                                                                 |otherwise = False                                                                                                                                                            

disparoIminenteC :: [Disparo] -> Jogador -> Bool
disparoIminenteC [] pl = False
disparoIminenteC ((DisparoCanhao n (c,l) d):bs) (Jogador (x,y) di vi g h) | (c == (x+1) && (l == (y+1) || l == (y-1))) = True
                                                                          | otherwise = False || disparoIminenteC bs (Jogador (x,y) di vi g h)
disparoIminenteC (_:bs) (Jogador (x,y) di vi g h) = False || disparoIminenteC bs (Jogador (x,y) di vi g h)    

disparoIminenteB :: [Disparo] -> Jogador -> Bool
disparoIminenteB [] pl = False
disparoIminenteB ((DisparoCanhao n (c,l) d):bs) (Jogador (x,y) di vi g h) | (c == (x-1) && (l == (y+1) || l == (y-1))) = True
                                                                          | otherwise = False || disparoIminenteB bs (Jogador (x,y) di vi g h)
disparoIminenteB (_:bs) (Jogador (x,y) di vi g h) = False || disparoIminenteB bs (Jogador (x,y) di vi g h)

disparoIminenteD :: [Disparo] -> Jogador -> Bool
disparoIminenteD [] pl = False
disparoIminenteD ((DisparoCanhao n (c,l) d):bs) (Jogador (x,y) di vi g h) | (l == (y+1) && (c == (x+1) || c == (x-1))) = True
                                                                          | otherwise = False || disparoIminenteD bs (Jogador (x,y) di vi g h)
disparoIminenteD (_:bs) (Jogador (x,y) di vi g h) = False || disparoIminenteD bs (Jogador (x,y) di vi g h)

disparoIminenteE :: [Disparo] -> Jogador -> Bool
disparoIminenteE [] pl = False
disparoIminenteE ((DisparoCanhao n (c,l) d):bs) (Jogador (x,y) di vi g h) | (l == (y-1) && (c == (x+1) || c == (x-1)))  = True
                                                                          | otherwise = False || disparoIminenteE bs (Jogador (x,y) di vi g h)
disparoIminenteE (_:bs) (Jogador (x,y) di vi g h) = False || disparoIminenteE bs (Jogador (x,y) di vi g h)

existeJogadorLinha :: [Jogador] -> Jogador -> Bool
existeJogadorLinha [] _ = False
existeJogadorLinha (b:bs) pl | pl == b = False
existeJogadorLinha ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) = if x == a then True
                                                                             else existeJogadorLinha t (Jogador (a,b) di vi l h)

existeJogadorColuna :: [Jogador] -> Jogador -> Bool
existeJogadorColuna [] _ = False
existeJogadorColuna (b:bs) pl | pl == b = False
existeJogadorColuna ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) = if y == b then True
                                                                             else existeJogadorColuna t (Jogador (a,b) di vi l h)


estaVoltadoLinha :: [Jogador] -> Jogador -> Bool
estaVoltadoLinha [] _ = False
estaVoltadoLinha ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) | existeJogadorLinha ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) && y > b && di == D = True
                                                                         | existeJogadorLinha ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) && y < b && di == E = True
                                                                         | otherwise = False

estaVoltadoColuna :: [Jogador] -> Jogador -> Bool
estoVoltadoColuna [] _ = False
estaVoltadoColuna ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) | existeJogadorColuna ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) && x > a && di == B = True
                                                                          | existeJogadorColuna ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) && x < a && di == C = True
                                                                          | otherwise = False


jogadorProximoC :: [Jogador] -> Jogador -> Bool
jogadorProximoC [] _ = False
jogadorProximoC (b:bs) pl | pl == b = False
jogadorProximoC ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) | x >= (a-2) && (y >= (b-2) || y <= (b+2)) = True
                                                                        | otherwise = False || jogadorProximoC t (Jogador (a,b) di vi l h) 

jogadorProximoB :: [Jogador] -> Jogador -> Bool
jogadorProximoB [] _ = False
jogadorProximoB (b:bs) pl | pl == b = False
jogadorProximoB ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) | x <= (a+2) && (y >= (b-2) || y <= (b+2)) = True
                                                                        | otherwise = False || jogadorProximoB t (Jogador (a,b) di vi l h)  

jogadorProximoD :: [Jogador] -> Jogador -> Bool
jogadorProximoD [] _ = False
jogadorProximoD (b:bs) pl | pl == b = False
jogadorProximoD ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) | y >= (b+2) && (x >= (a-2) || x <= (a+2)) = True
                                                                        | otherwise = False || jogadorProximoD t (Jogador (a,b) di vi l h)

jogadorProximoE :: [Jogador] -> Jogador -> Bool
jogadorProximoE [] _ = False
jogadorProximoE (b:bs) pl | pl == b = False
jogadorProximoE ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) | y >= (b-2) && (x >= (a-2) || x <= (a+2)) = True
                                                                        | otherwise = False || jogadorProximoE t (Jogador (a,b) di vi l h)

jogadorBot :: [Jogador] -> Int -> Jogador
jogadorBot [b] _ = b
jogadorBot (b:bs) n = if n == 0 then b
                      else jogadorBot bs (n-1)

posicaoC :: Jogador -> (Int,Int)
posicaoC (Jogador (x,y) d v cl ch) = (x-1,y)

posicaoB :: Jogador -> (Int,Int)
posicaoB (Jogador (x,y) d v cl ch) = (x+1,y)

posicaoE :: Jogador -> (Int,Int)
posicaoE (Jogador (x,y) d v cl ch) = (x,y-1)

posicaoD :: Jogador -> (Int,Int)
posicaoD (Jogador (x,y) d v cl ch) = (x,y+1)   

direcaoBot :: Jogador -> Direcao 
direcaoBot (Jogador (x,y) d v cl ch) = d 

temlaser :: Jogador -> Bool
temlaser (Jogador (x,y) d v cl ch) = if cl > 0 then True
                                     else False

temChoque :: Jogador -> Bool
temChoque (Jogador (x,y) d v cl ch) = if ch > 0 then True
                                     else False

jogadorProximo :: [Jogador] -> Jogador -> Bool
jogadorProximo [] _ = False
jogadorProximo (b:bs) pl | pl == b = False
jogadorProximo ((Jogador (x,y) d v cl ch):t) (Jogador (a,b) di vi l h) | x == (a-3) && (y == (b-3) || y == (b+3)) || x == (a+3) && (y == (b-3) || y == (b+3)) ||  y == (b+3) && (x == (a-3) || x == (a+3)) || y == (b-3) && (x == (a-3) || x == (a+3)) = True
                                                                        | otherwise = False || jogadorProximo t (Jogador (a,b) di vi l h)                                      