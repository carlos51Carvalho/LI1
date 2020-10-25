
{- |Module      : Tarefa5_2018li1g146
    Description : Construir a parte gráfica do nosso Jogo 
    Copyright   : Carlos Carvalho <a89605@alunos.uminho.pt>
                   Fernando Lopes <a89472@alunos.uminho.pt> 

= Resumo:
Nesta Tarefa começamos por desenhar o mapa na janela, jogadores e disparos a eles correspondentes de seguida tratamos apenas de fazer com que os jogadores reagissem a comandos 
e por ultimo tratamos de criar um menu com possiblidade de modo single player o multyplayer e ainda possiblidade de esolher o mapa a utilizar.
Conseguimos assim realizar a tarefa mas com alguns defeits devido a tarefas anteriores.  



= Introdução:
Nesta Tarefa o objetivo era construir e projetar o nosso jogo, ou seja , juntar de alguma forma todas as nossas tarefas realizadas anteriormente numa só ,
para isso utilizámos o Gloss ferramenta que nos primitiu alguma liberdade na animação e design do jogo. Uma das maiores dificuldades desta Tarefa foi talvez perceber o gloss e a forma de funcionamento,
e lidar com possiveis erros que podessem existir em Tarefas anteriores.


= Objetivos e estratégias:
Numa fase Inicial começamos por tentar compreender o gloss, para isso utilizamos o guiao 3 fornecido pelos professores como ponto de partida, de seguida uma das primerias coisas a fazer foi construir o mapa,
para isso, criamos um contador de colunas que foi depois empregue na função desenhaMapa com o intuito de saber quantas imagens esta teria de imprimir antes de mudar de linha, sendo que quando o contador chegava a 0
a função deveria mudar de linha ( neste caso alterar o valor do x e do y posiçoes relativas da imagem no ecra), de seguida tivemos de desenhar os jogadores dentro do mapa, para isso criamos uma função que atraves de uma lista 
de jogadores uma lista de imagens e uma posiçao relativa (posiçºao onde começa o mapa a ser desenhado), aqui ao passar a imagem para o mapa tivemos de ter em conta a posição do jogador e as posiçoes relativas do mapa 
sendo que como que imaginamos que o x y eram como o inicio de um referencial (0,0) e as posiçoes dos jogadores (depois de passadas para o tipo float e serem adequadas a mediada de lado de cada componente do mapa)
tomariam o valor de (x + (posiçao do jogador)) e (y - (posiçao do jogador)). Ainda na desenhaJogadores tivemos de ter atenção a direçao de cada jogador sendo que as nossas imagens consoante a direço mudam de prespetiva.
Por fim antes de aplicar tudo numa desenha estado criamos a desenhaDisparos que é a função que desenha os disparos, esta tem sobretudo em atenção o Jogador que dispara (visto os disparos terem cores diferentes para cada jogador), 
e o tipo de disparo,  na parte de desenhar canhoes foi praticamente identico á desenha jogadores tendo apenas de ter em consideração a posição do jogador que disparava para desenhar o disparo, para o desenho 
dos Lasers numa parte inicial o processo foi o mesmo do desenho dos canhoes tendo apenas que ter em conta se o laser tinha de ser desenhado verticalmente ou horizontalmente (para isso tivemos de ter atenção á direção do jogador),
mais tarde chegamos a conclusao que tinhamos que desenhar o laser em todas as posições possiveis de ele ocupar parando apenas quando embatia num bloco Indestrutivel, e para isso a nossa estratégia foi armazenar
consoante a direção em que o laser era disparado as posiçoes que ele podia tomar ( com a função armazenaLaser) e postriormente criar uma função que desenhasse o Laser em todas essas posições a continuaLaser.
Por ultimo no desenho dos disparos, o desenho do choque muito semelhante aos anteriores tivemos apenas que ter em conta a posição do jogador de forma a que a imagem correspondente ao choque aparecesse na posição devida.
Ainda nos desenhos criamos várias funçoes que apenas colocam imagens em determinados sitios e se alteram consoante os estados dos jogadores (numero de vidas, lasers choques), imagens estas que indicam ao utilizador a situação dos seus avatars.
Por fim juntamos tudo isto (dependente de um Estado) na função desenhaEstado que ao receber um Estado e todas as Pictures a usar em todas as funçoes auxiliares transforma toda esta infrmação numa Picture.
Mais tarde foi vez de tratarmos dos eventos que acontecem e que permitem efetivamente ao utilizador criar o jogo, começamos por criar um EstadoGloss que recebesse não só a informação dada por um dao Estado mas também
um inteiro identificardor (mais tarde usado na desenhaEstado referenciada em cima para desenvolver os menus) mas tamém uma lista de Keys (possiveis Keys a ser pressionadas pelo utilizador) de seguida criamos uma lista de várias teclas 
que qaundo pressionadas fazem com que o estado se altere, ou a posição do jogador ou os disparos, relativamente a posição dos joogadores tivemos que ir definindo de forma a que quando a tecla fosse pressionada esta tecla fosse 
armazenada na Lista de keys enquanto estivesse a ser pressionada, quando o utilizador deixa de pressionar a tecla esta é removida da lista de keys pela funçao remove Key, quanto á deslocação do jogador em si, 
esta é efetuada quando a tecla é pressionada sendo alterado o estado inicial pelo evento ( ou seja o jogador altera a poisção) através da função aplicaListaKey incumbida na função reage tempo. Quanto aos evetos dos disparos
foi apenas uma questão de uma vez pressionados se dar uma alteração do estado (efetuando um disparo).
Relativamente aos Menus criamos vários estados alguns deles vazios atribuindo lhes Ints para os diferenciar na desenha de froma a que na desenhaEstado estes fossem reconhecidos e desenhados como tal, ou seja lhes fossem aplicadas 
as imagens respetivas.

== Exemplos :
 Exemplo onde é possivel verificar a construção de vários estados com diferentes numeros de jogadore e onde existem alterações nos retangulos que representam os avatares
<<imagens/imagem.png>> 
<<imagens/imagem2.png>> 
<<imagens/imagem3.png>>


= Conclusão: 
Para concluir, conseguimos representar o nosso jogo em termos graficos contudo devido a alguns erros em tarefas anteriores não ficou totalmente bem conseguida, queriamos ainda
incluir vários modos de jogo o multy e o single player contudo não conseguimos .


-}

-- | Este módulo define funções comuns da Tarefa 5 do trabalho prático.
module Main where

import LI11819
import Tarefa0_2018li1g146
import Tarefa1_2018li1g146
import Tarefa2_2018li1g146
import Tarefa3_2018li1g146
import Tarefa4_2018li1g146
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
-- | Função principal da Tarefa 5.
--
-- __NB:__ Esta Tarefa é completamente livre. Deve utilizar a biblioteca <http://hackage.haskell.org/package/gloss gloss> para animar o jogo, e reutilizar __de forma completa__ as funções das tarefas anteriores.
-- * Estado Jogo

--type Estado = (Bool,Float,Float)



-- * Estados Vários

main :: IO ()
main = do {
       ab <- loadBMP "avengers-inf.bmp";
       civ<- loadBMP "civil.bmp";
       men<- loadBMP "menu.bmp";
       map<- loadBMP "mapas.bmp"; 
       y <- loadBMP "indestrutivel.bmp";
       vz <- loadBMP "vazia.bmp";
       d <- loadBMP "destrutivel.bmp";
       cd <- loadBMP "gemar.bmp";
       ci <- loadBMP "gemay.bmp";
       cs <- loadBMP "gemap.bmp";
       ct <- loadBMP "gemab.bmp";
       ld <- loadBMP "laserd.bmp";
       li <- loadBMP "laseri.bmp";
       lt <- loadBMP "lasert.bmp";
       ls <- loadBMP "lasers.bmp";
       chd<- loadBMP "chd.bmp";
       chi<- loadBMP "chi.bmp";
       cht<- loadBMP "cht.bmp";
       chs<- loadBMP "chs.bmp";
       rD <- loadBMP "retD.bmp";
       rI <- loadBMP "retI.bmp";
       rT <- loadBMP "retT.bmp";
       rS <- loadBMP "retS.bmp";
       rC <- loadBMP "retC.bmp";
       v3 <- loadBMP "3vidas.bmp";
       v2 <- loadBMP "2vidas.bmp";
       v1 <- loadBMP "vidas.bmp";
       t3 <- loadBMP "choque3.bmp";
       t2 <- loadBMP "choque2.bmp";
       t1 <- loadBMP "choque1.bmp";
       t0 <- loadBMP "choque0.bmp";
       ls3<- loadBMP "laser3.bmp";
       ls2<- loadBMP "laser2.bmp";
       ls1<- loadBMP "laser1.bmp";
       ls0<- loadBMP "laser0.bmp";
       Just x <- loadJuicy "marvel.png";
       Just v0 <- loadJuicy "x.png";
       Just db <- loadJuicy "dpb.png";
       Just dc <- loadJuicy "dpc.png";
       Just dd <- loadJuicy "dpd.png";
       Just de <- loadJuicy "dpe.png";
       Just imc <- loadJuicy "imc.png";
       Just imd <- loadJuicy "imd.png";
       Just imb <- loadJuicy "imb.png";
       Just ime <- loadJuicy "ime.png";
       Just tc <- loadJuicy "tc.png";
       Just td <- loadJuicy "td.png";
       Just tb <- loadJuicy "tb.png";
       Just te <- loadJuicy "te.png";
       Just stc <- loadJuicy "stc.png";
       Just std <- loadJuicy "std.png" ;
       Just stb <- loadJuicy "stb.png";
       Just ste <- loadJuicy "ste.png";
       Just capc <- loadJuicy "capc.png";
       Just capd <- loadJuicy "capd.png";
       Just capb <- loadJuicy "capb.png";
       Just cape<- loadJuicy "cape.png";
       play dm                       -- janela onde irÃ¡ correr o jogo
            (greyN 0.5)               -- cÃ´r do fundo da janela
            fr                        -- frame rate
            estadoGlossInicial        -- estado inicial
            (desenhaEstado [ab,men,map,civ,x,rD,rI,rT,rS,rC,v3,v2,v1,v0,t3,t2,t1,t0,ls3,ls2,ls1,ls0,y,d,vz,cd,ci,ct,cs,ld,li,lt,ls,chd,chi,cht,chs,dc,dd,db,de,imc,imd,imb,ime,tc,td,tb,te,stc,std,stb,ste,capc,capd,capb,cape])      -- desenha o estado do jogo
            reageEventoGloss          -- reage a um evento
            reageTempoGloss     }      -- reage ao passar do tempo

mapa2 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]

mapa3 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Vazia,Bloco Destrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Vazia,Vazia,Bloco Destrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]

jogador_1 = (Jogador (14,1) B 7 10 10)
jogador_2 = (Jogador (1,15) C 3 3 3)
jogador_3 = (Jogador (14,15) E 1 3 3)
jogador_4 = (Jogador (1,1) D 3 3 3)



--estados multiplayer

estado3 :: Estado
estado3 = Estado m j ds
              where m = mapa3
                    j = [jogador_1, jogador_2, jogador_3,jogador_4] 
                    ds = []


estado2 :: Estado
estado2 = Estado m j ds
              where m = mapa2
                    j = [jogador_1, jogador_2, jogador_3,jogador_4] 
                    ds = []


estado1 :: Estado
estado1 = Estado m j ds
              where m = mapa1
                    j = [jogador_1, jogador_2, jogador_3,jogador_4] 
                    ds = []


-- estados singular
estado_1 :: Estado
estado_1 = Estado m j ds
              where m = mapa1
                    j = [jogador_1, jogador_2] 
                    ds = []

estado_2 :: Estado
estado_2 = Estado m j ds
              where m = mapa2
                    j = [jogador_1, jogador_2] 
                    ds = []

estado_3 :: Estado
estado_3 = Estado m j ds
              where m = mapa3
                    j = [jogador_1, jogador_2] 
                    ds = []



-- estado para o mapas e menu 

estadoneutro :: Estado
estadoneutro = Estado m j ds
              where m = []
                    j = [] 
                    ds = []



-- | lado de cada quadricula
lado :: Float
lado = 30

-- | bloco preto == vazia
vazia:: Picture
vazia = (Polygon [(0,0),(lado,0),(lado,lado),(0,lado),(0,0)])
 

-- | conta as colunas do mapa 
contadorColunas :: Mapa -- ^ Recebe um mapa 
                -> Int -- ^ Dá um Int , o numero de colunas
contadorColunas [] = 0
contadorColunas m = length (head m)  


-- | Função que transforma o mapa dado numa lista de Pictures 
desenhoMapa :: Picture -- ^ Recebe uma Picture
            -> Picture -- ^ Recebe uma Picture
            -> Picture  -- ^ Recebe uma Pictue
            -> Int -- ^ Numero de colunas de forma a saber quando mudar de linha
            -> Mapa -- ^ Recebe um 'Mapa'
            -> [Peca] -- ^ Recebe Linhas do mapa
            -> Float -- ^ Posição a aplicar no ecra eixo do x
            -> Float  -- ^ POsicao a aplicar no ecra eixo dos y
            -> [Picture] -- ^ Dá uma lista de 'Picture'
desenhoMapa _ _ _ _ _ [] _ _ = []
desenhoMapa v d bI 0 m l x y = desenhoMapa v d bI (contadorColunas m) m l (x-lado*(realToFrac(contadorColunas m))) (y-lado)
desenhoMapa v d bI c m (h:t) x y | h == Bloco Indestrutivel = (Translate (x-15) (y+15) bI): (desenhoMapa v d bI (c-1) m t (x+lado) y)   
                                 | h == Bloco Destrutivel = (Translate (x-15) (y+15) d): (desenhoMapa v d bI (c-1) m t (x+lado)  y) 
                                 | otherwise = (Translate (x-30) y vazia): (desenhoMapa v d bI (c-1) m t (x+lado)  y)  




-- | Funçao que sejenha os jogadores com as suas respetivas imagens na sua respetiva Posicao
desenhaJogadores :: [Jogador] -- ^ Recebe a lista Jogadores a ser aplicada
                 -> Float -- ^ Posicao a aplicar a imagem no ecra
                 -> Float -- ^ Posicao a aplicar a imagem no ecra
                 -> [Picture] -- ^ Recebe uma lista de 'Picture' que vai aplicar  
                 -> [Picture] -- ^ LIsta de Jogadores ja em 'Picture'
desenhaJogadores [] _ _ _ = []
desenhaJogadores ((Jogador _ _ 0 _ _):t) x y (pc:pd:pb:pe:ps) = desenhaJogadores t x y ps
desenhaJogadores ((Jogador (a,b) d v l c):t) x y (pc:pd:pb:pe:ps) | d == C = ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado)  pc) : (desenhaJogadores t x y ps))
                                                                  | d == D = ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado)  pd) : (desenhaJogadores t x y ps))
                                                                  | d == B = ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado)  pb) : (desenhaJogadores t x y ps))
                                                                  | d == E = ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado)  pe) : (desenhaJogadores t x y ps))
desenhaJogadores _ _ _ _ = []




-- | Função que desenha os disparos e lhes aplica a cada  um uma determinada imagem tendo em conta o jogador que efetua o disparo
desenhaDisparos :: Mapa -- ^ Recebe um mapa
                -> [Jogador] -- ^ Recebe a lista de jogadores que podem efetuar disparos
                -> [Disparo] -- ^ Recebe a lista de disparos que os Jogadores pretendem fazer
                -> Float -- ^ Posicao a aplicar a imagem no ecra
                -> Float -- ^ Posicao a aplicar a imagem no ecra
                -> [Picture] -- ^ Lista de imagens que os canhões vao poder tomar de acordo com o Jogador que efetua o disparo
                -> [Picture] -- ^ Lista de imagens que os Laser vao poder tomar de acordo com o Jogador que efetua o disparo
                -> [Picture] -- ^ Lista de imagens que os Choques vao poder tomar de acordo com o Jogador que efetua o disparo 
                -> [Picture]
desenhaDisparos m j ((DisparoCanhao 0 (a,b) d):ty) x y (cd:r) l ch = ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado)  cd) : (desenhaDisparos m j ty x y (cd:r) l ch))
desenhaDisparos m j ((DisparoCanhao 1 (a,b) d):ty) x y (cd:ci:r) l ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado)  ci) : (desenhaDisparos m j ty x y (cd:ci:r) l ch))
desenhaDisparos m j ((DisparoCanhao 2 (a,b) d):ty) x y (cd:ci:ct:r) l ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado)  ct) : (desenhaDisparos m j ty x y (cd:ci:ct:r) l ch))
desenhaDisparos m j ((DisparoCanhao 3 (a,b) d):ty) x y (cd:ci:ct:cs:r) l ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado)  cs) : (desenhaDisparos m j ty x y (cd:ci:ct:cs:r) l ch))

desenhaDisparos m j ((DisparoLaser 0 (a,b) C):t) x y (c:r) (l:u) ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) (Rotate 90 l)): (continuaLaser x y (armazenaLaserC (a,b) m) (Rotate 90 l))) ++ (desenhaDisparos m j t x y (c:r) (l:u) ch) 
desenhaDisparos m j ((DisparoLaser 0 (a,b) B):t) x y (c:r) (l:u) ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) (Rotate 90 l)): (continuaLaser x y (armazenaLaserB (a,b) m) (Rotate 90 l))) ++ (desenhaDisparos m j t x y (c:r) (l:u) ch) 
desenhaDisparos m j ((DisparoLaser 0 (a,b) E):t) x y (c:r) (l:u) ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) l): (continuaLaser x y (armazenaLaserE (a,b) m) l)) ++ (desenhaDisparos m j t x y (c:r) (l:u) ch) 
desenhaDisparos m j ((DisparoLaser 0 (a,b) D):t) x y (c:r) (l:u) ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) l): (continuaLaser x y (armazenaLaserD (a,b) m) l)) ++ (desenhaDisparos m j t x y (c:r) (l:u) ch) 

desenhaDisparos m j ((DisparoLaser 1 (a,b) C):t) x y (c:r) (l:l1:u) ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) (Rotate 90 l1)): (continuaLaser x y (armazenaLaserC (a,b) m) (Rotate 90 l1))) ++ (desenhaDisparos m j t x y (c:r) (l:l1:u) ch) 
desenhaDisparos m j ((DisparoLaser 1 (a,b) B):t) x y (c:r) (l:l1:u) ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) (Rotate 90 l1)): (continuaLaser x y (armazenaLaserB (a,b) m) (Rotate 90 l1))) ++ (desenhaDisparos m j t x y (c:r) (l:l1:u) ch) 
desenhaDisparos m j ((DisparoLaser 1 (a,b) E):t) x y (c:r) (l:l1:u) ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) l1): (continuaLaser x y (armazenaLaserE (a,b) m) l1)) ++ (desenhaDisparos m j t x y (c:r) (l:l1:u) ch) 
desenhaDisparos m j ((DisparoLaser 1 (a,b) D):t) x y (c:r) (l:l1:u) ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) l1): (continuaLaser x y (armazenaLaserD (a,b) m) l1)) ++ (desenhaDisparos m j t x y (c:r) (l:l1:u) ch) 

desenhaDisparos m j ((DisparoLaser 2 (a,b) C):t) x y (c:r) (l:l1:l2:u) ch = ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) (Rotate 90 l2)): (continuaLaser x y (armazenaLaserC (a,b) m) (Rotate 90 l2))) ++ (desenhaDisparos m j t x y (c:r) (l:l1:l2:u) ch) 
desenhaDisparos m j ((DisparoLaser 2 (a,b) B):t) x y (c:r) (l:l1:l2:u) ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) (Rotate 90 l2)): (continuaLaser x y (armazenaLaserB (a,b) m) (Rotate 90 l2))) ++ (desenhaDisparos m j t x y (c:r) (l:l1:l2:u) ch)
desenhaDisparos m j ((DisparoLaser 2 (a,b) E):t) x y (c:r) (l:l1:l2:u) ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) l2): (continuaLaser x y (armazenaLaserE (a,b) m) l2)) ++ (desenhaDisparos m j t x y (c:r) (l:l1:l2:u) ch) 
desenhaDisparos m j ((DisparoLaser 2 (a,b) D):t) x y (c:r) (l:l1:l2:u) ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) l2): (continuaLaser x y (armazenaLaserD (a,b) m) l2)) ++ (desenhaDisparos m j t x y (c:r) (l:l1:l2:u) ch) 

desenhaDisparos m j ((DisparoLaser 3 (a,b) C):t) x y (c:r) (l:l1:l2:l3:u) ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) (Rotate 90 l3)): (continuaLaser x y (armazenaLaserC (a,b) m) (Rotate 90 l3))) ++ (desenhaDisparos m j t x y (c:r) (l:l1:l2:l3:u) ch)
desenhaDisparos m j ((DisparoLaser 3 (a,b) B):t) x y (c:r) (l:l1:l2:l3:u) ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) (Rotate 90 l3)): (continuaLaser x y (armazenaLaserB (a,b) m) (Rotate 90 l3))) ++ (desenhaDisparos m j t x y (c:r) (l:l1:l2:l3:u) ch)
desenhaDisparos m j ((DisparoLaser 3 (a,b) E):t) x y (c:r) (l:l1:l2:l3:u) ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) l3): (continuaLaser x y (armazenaLaserE (a,b) m) l3)) ++ (desenhaDisparos m j t x y (c:r) (l:l1:l2:l3:u) ch) 
desenhaDisparos m j ((DisparoLaser 3 (a,b) D):t) x y (c:r) (l:l1:l2:l3:u) ch= ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) l3): (continuaLaser x y (armazenaLaserD (a,b) m) l3)) ++ (desenhaDisparos m j t x y (c:r) (l:l1:l2:l3:u) ch) 

desenhaDisparos m ((Jogador (a,b) d v ls ch):js) ((DisparoChoque 0 tic ):t) x y c l (k:ks)  = ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) k):(desenhaDisparos m ((Jogador (a,b) d v ls ch):js) t x y c l (k:ks)))
desenhaDisparos m (j:(Jogador (a,b) d v ls ch):js) ((DisparoChoque 1 tic ):t) x y c l (k:k1:ks) = ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) k1):(desenhaDisparos m (j:(Jogador (a,b) d v ls ch):js) t x y c l (k:k1:ks)))
desenhaDisparos m (j:j1:(Jogador (a,b) d v ls ch):js) ((DisparoChoque 2 tic ):t) x y c l (k:k1:k2:ks) = ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) k2):(desenhaDisparos m (j:j1:(Jogador (a,b) d v ls ch):js) t x y c l (k:k1:k2:ks)))
desenhaDisparos m (j:j1:j2:(Jogador (a,b) d v ls ch):js) ((DisparoChoque 3 tic ):t) x y c l (k:k1:k2:k3:ks) = ((Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) k3):(desenhaDisparos m (j:j1:j2:(Jogador (a,b) d v ls ch):js) t x y c l (k:k1:k2:k3:ks)))

desenhaDisparos _ _ _ _ _ _ _ _= []


-- | Função que armazena todas as Posicao que o Laser pode percorrer num disparo para Cima
armazenaLaserC :: (Int,Int) -- ^ Posicao de onde é efetuado o primeiro disparo 
               -> Mapa -- ^ Recebe um Mapa
               -> [(Int,Int)] -- ^ Lista de Posicao que o Laser pode percorrer no disparo 
armazenaLaserC (a,b) m | (encontraPosicaoMatriz (a-1, b) m)  /= Bloco Indestrutivel && (encontraPosicaoMatriz (a-1,b+1) m) /= Bloco Indestrutivel  = (a-1, b) : (armazenaLaserC (a-1,b) m )
                       | otherwise = [] 

-- | Função que armazena todas as Posicao que o Laser pode percorrer num disparo para Baixo
armazenaLaserB :: (Int,Int) -- ^ Posicao de onde é efetuado o primeiro disparo 
               -> Mapa -- ^ Recebe um Mapa
               -> [(Int,Int)] -- ^ Lista de Posicao que o Laser pode percorrer no disparo
armazenaLaserB (a,b) m | (encontraPosicaoMatriz (a+1, b) m)  /= Bloco Indestrutivel && (encontraPosicaoMatriz (a+1,b+1) m) /= Bloco Indestrutivel  = (a+1, b) : (armazenaLaserB (a+1,b) m )
                       | otherwise = [] 

-- | Função que armazena todas as Posicao que o Laser pode percorrer num disparo para a Esquerda
armazenaLaserE :: (Int,Int) -- ^ Posicao de onde é efetuado o primeiro disparo
               -> Mapa -- ^ Recebe um Mapa
               -> [(Int,Int)]-- ^ Lista de Posicao que o Laser pode percorrer no disparo
armazenaLaserE (a,b) m | (encontraPosicaoMatriz (a, b-1) m)  /= Bloco Indestrutivel && (encontraPosicaoMatriz (a-1,b-1) m) /= Bloco Indestrutivel  = (a, b-1) : (armazenaLaserE (a,b-1) m )
                       | otherwise = [] 

-- | Função que armazena todas as Posicao que o Laser pode percorrer num disparo para a Direita
armazenaLaserD :: (Int,Int) -- ^ Posicao de onde é efetuado o primeiro disparo
               -> Mapa -- ^ Recebe um Mapa
               -> [(Int,Int)] -- ^ Lista de Posicao que o Laser pode percorrer no disparo
armazenaLaserD (a,b) m | (encontraPosicaoMatriz (a, b+1) m)  /= Bloco Indestrutivel && (encontraPosicaoMatriz (a+1,b+1) m) /= Bloco Indestrutivel  = (a, b+1) : (armazenaLaserD (a,b+1) m )
                       | otherwise = [] 


-- | Função que tendo todas as Posicao wue o Laser pode tomar impplementa a respetiva imagem nessas posicao
continuaLaser :: Float -- ^ Posicao a aplicar a imagem no ecra
              -> Float  -- ^ Posicao a aplicar a imagem no ecra
              ->[(Int,Int)] -- ^ Lista de Posicao que o Laser pode percorrer no disparo
              -> Picture -- Imagem respetiva ao Laser de um cert Jogador 
              -> [Picture] -- ^Lista de Imagens com todas as Posiçoes ocupadas pelo disparo
continuaLaser _ _ [] _ = []
continuaLaser x y ((a,b):t) img = (Translate (x + (realToFrac b)* lado) (y - (realToFrac a)*lado) img) : (continuaLaser x y t img)




-- | Função que desenha os retangulos onde esta a vida os chques e os lasers dos Jogadores
retangulosVida:: Float -- ^ Posicao a aplicar a imagem no ecra
              -> Float -- ^ Posicao a aplicar a imagem no ecra
              -> [Picture] -- ^ Lista de Retangulos dos diferents Jogador
              -> [Picture] -- ^ Lista de Retangulos aplicada
retangulosVida x y [] = []
retangulosVida x y (r:t) = (Translate x y r) : (retangulosVida x (y-250) t)


-- | Função que desenha a vida de cada Jogador no Retagulo defindo na função a cima, caso a vida seja superior a 3 ele mostra apenas 3 coraçoes 
vida :: Float -- ^ Posicao a aplicar a imagem no ecra
     -> Float -- ^ Posicao a aplicar a imagem no ecra
     -> [Picture] -- ^ LIsta daos possiveis estados de vida de cada jogador (3 coraçoes , 2 coraçoes,1 coraçao ou morto)
     -> [Jogador] -- ^ Lista dos Jogadores sujeitos a mediçao de vidas
     -> [Picture] -- ^ Lista de Imagens que aparecem no ecra conforme o numero de vidas de cada Jogador
vida _ _ _ [] = []
vida x y (v3:v2:v1:v0:m) ((Jogador (a,b) d v l c ):t) | v == 3 = (Translate x y v3 ): (vida x (y-250) (v3:v2:v1:v0:m) t)
                                                      | v == 2 = (Translate x y v2 ): (vida x (y-250) (v3:v2:v1:v0:m) t)
                                                      | v == 1 = (Translate x y v1 ): (vida x (y-250) (v3:v2:v1:v0:m) t) 
                                                      | v == 0 =(Translate x y v0): (vida x (y-250) (v3:v2:v1:v0:m) t)
                                                      | otherwise = (Translate x y v3 ): (vida x (y-250) (v3:v2:v1:v0:m) t)  

-- | Funçao que desenha o numero de choques de cada jogador,e caso superior a 3 mostra apenas 3 choques
choques:: Float -- ^ Posicao a aplicar a imagem no ecra
       -> Float -- ^ Posicao a aplicar a imagem no ecra
       -> [Picture] -- ^ Lista de possiveis estados dos Choques para os vários Jogadores
       -> [Jogador] -- ^ Lista de Jogadores sujeita a mediçao de disparos 
       -> [Picture]-- ^ Lista de Imagens que aparecem no ecra conforme o numero de Choques de cada Jogador
choques _ _ _ [] = []
choques x y (c3:c2:c1:c0:m)  ((Jogador (a,b) d v l c ):t) | c == 3 = (Translate x y c3 ): (choques x (y-250) (c3:c2:c1:c0:m) t)
                                                          | c == 2 = (Translate x y c2 ): (choques x (y-250) (c3:c2:c1:c0:m) t)
                                                          | c == 1 = (Translate x y c1 ): (choques x (y-250) (c3:c2:c1:c0:m) t) 
                                                          | c == 0 = (Translate x y c0): (choques x (y-250) (c3:c2:c1:c0:m) t)
                                                          | otherwise = (Translate x y c3): (choques x (y-250) (c3:c2:c1:c0:m) t)

-- | Funçao que desenha o numero de laser de cada jogador,e caso superior a 3 mostra apenas 3 laser
laser :: Float -- ^ Posicao a aplicar a imagem no ecra
      -> Float -- ^ Posicao a aplicar a imagem no ecra
      -> [Picture] -- ^ Lista de possiveis estados dos Laser para os vários Jogadores
      -> [Jogador]  -- ^ Lista de Jogadores sujeita a mediçao de disparos 
      -> [Picture]-- ^ Lista de Imagens que aparecem no ecra conforme o numero de Laser de cada Jogador
laser _ _ _ [] = []
laser x y (ls3:ls2:ls1:ls0:m)  ((Jogador (a,b) d v l c ):t) | l == 3 = (Translate x y ls3 ): (laser x (y-250) (ls3:ls2:ls1:ls0:m) t)
                                                            | l == 2 = (Translate x y ls2 ): (laser x (y-250) (ls3:ls2:ls1:ls0:m) t)
                                                            | l == 1 = (Translate x y ls1 ): (laser x (y-250) (ls3:ls2:ls1:ls0:m) t)
                                                            | l == 0 = (Translate x y ls0):(laser x (y-250) (ls3:ls2:ls1:ls0:m) t)
                                                            | otherwise = (Translate x y ls3 ):(laser x (y-250) (ls3:ls2:ls1:ls0:m) t)

 



-- | Funçao que junta todas as Imagens obtidas atraves das outras desenhas desenhando assim o Estado
desenhaEstado :: [Picture] -- ^ Lista de todas as imagens usados para desenhar qualquer um dos possiveis estados
              -> EstadoGloss -- ^ Recebe um estado numerado 
              -> Picture -- ^ Dá apenas uma só imagem 
desenhaEstado (ab:men:map:civ:x:rD:rI:rT:rS:rC:v3:v2:v1:v0:t3:t2:t1:t0:ls3:ls2:ls1:ls0:y:d:vz:cd:ci:ct:cs:ld:li:lt:ls:chd:chi:cht:chs:dc:dd:db:de:imc:imd:imb:ime:tc:td:tb:te:stc:std:stb:ste:capc:capd:capb:cape:t) (0,(Estado m lj disp),e) = Pictures $ [ab] 
desenhaEstado (ab:men:map:civ:x:rD:rI:rT:rS:rC:v3:v2:v1:v0:t3:t2:t1:t0:ls3:ls2:ls1:ls0:y:d:vz:cd:ci:ct:cs:ld:li:lt:ls:chd:chi:cht:chs:dc:dd:db:de:imc:imd:imb:ime:tc:td:tb:te:stc:std:stb:ste:capc:capd:capb:cape:t) (1,(Estado m lj disp),e) = Pictures $ [men]
desenhaEstado (ab:men:map:civ:x:rD:rI:rT:rS:rC:v3:v2:v1:v0:t3:t2:t1:t0:ls3:ls2:ls1:ls0:y:d:vz:cd:ci:ct:cs:ld:li:lt:ls:chd:chi:cht:chs:dc:dd:db:de:imc:imd:imb:ime:tc:td:tb:te:stc:std:stb:ste:capc:capd:capb:cape:t) (2,(Estado m lj disp),e) = Pictures $ [map] 
desenhaEstado (ab:men:map:civ:x:rD:rI:rT:rS:rC:v3:v2:v1:v0:t3:t2:t1:t0:ls3:ls2:ls1:ls0:y:d:vz:cd:ci:ct:cs:ld:li:lt:ls:chd:chi:cht:chs:dc:dd:db:de:imc:imd:imb:ime:tc:td:tb:te:stc:std:stb:ste:capc:capd:capb:cape:t) (3,(Estado m lj disp),e) = Pictures $ [x] ++ (retangulosVida (600) 400 (rD:rI:rT:rS:[])) ++ (choques 600 400 (t3:t2:t1:t0:[]) lj) ++ (laser 750 405 (ls3:ls2:ls1:ls0:[]) lj) ++ (vida 680 400 (v3:v2:v1:v0:[]) lj) ++ (desenhoMapa vz d y (contadorColunas m) m (concat m) (-300) 360) ++ (desenhaJogadores lj (-300) 360 (dc:dd:db:de:imc:imd:imb:ime:tc:td:tb:te:stc:std:stb:ste:[])) ++ (desenhaDisparos m lj disp (-300) 360 (cd:ci:ct:cs:[]) (ld:li:lt:ls:[]) (chd:chi:cht:chs:[]))
desenhaEstado (ab:men:map:civ:x:rD:rI:rT:rS:rC:v3:v2:v1:v0:t3:t2:t1:t0:ls3:ls2:ls1:ls0:y:d:vz:cd:ci:ct:cs:ld:li:lt:ls:chd:chi:cht:chs:dc:dd:db:de:imc:imd:imb:ime:tc:td:tb:te:stc:std:stb:ste:capc:capd:capb:cape:t) (4,(Estado m lj disp),e) = Pictures $ [x] ++ (retangulosVida (600) 400 (rD:rI:rT:rS:[])) ++ (choques 600 400 (t3:t2:t1:t0:[]) lj) ++ (laser 750 405 (ls3:ls2:ls1:ls0:[]) lj) ++ (vida 680 400 (v3:v2:v1:v0:[]) lj) ++ (desenhoMapa vz d y (contadorColunas m) m (concat m) (-300) 360) ++ (desenhaJogadores lj (-300) 360 (dc:dd:db:de:imc:imd:imb:ime:tc:td:tb:te:stc:std:stb:ste:[])) ++ (desenhaDisparos m lj disp (-300) 360 (cd:ci:ct:cs:[]) (ld:li:lt:ls:[]) (chd:chi:cht:chs:[]))
desenhaEstado (ab:men:map:civ:x:rD:rI:rT:rS:rC:v3:v2:v1:v0:t3:t2:t1:t0:ls3:ls2:ls1:ls0:y:d:vz:cd:ci:ct:cs:ld:li:lt:ls:chd:chi:cht:chs:dc:dd:db:de:imc:imd:imb:ime:tc:td:tb:te:stc:std:stb:ste:capc:capd:capb:cape:t) (5,(Estado m lj disp),e) = Pictures $ [x] ++ (retangulosVida (600) 400 (rD:rI:rT:rS:[])) ++ (choques 600 400 (t3:t2:t1:t0:[]) lj) ++ (laser 750 405 (ls3:ls2:ls1:ls0:[]) lj) ++ (vida 680 400 (v3:v2:v1:v0:[]) lj) ++ (desenhoMapa vz d y (contadorColunas m) m (concat m) (-300) 360) ++ (desenhaJogadores lj (-300) 360 (dc:dd:db:de:imc:imd:imb:ime:tc:td:tb:te:stc:std:stb:ste:[])) ++ (desenhaDisparos m lj disp (-300) 360 (cd:ci:ct:cs:[]) (ld:li:lt:ls:[]) (chd:chi:cht:chs:[]))
desenhaEstado (ab:men:map:civ:x:rD:rI:rT:rS:rC:v3:v2:v1:v0:t3:t2:t1:t0:ls3:ls2:ls1:ls0:y:d:vz:cd:ci:ct:cs:ld:li:lt:ls:chd:chi:cht:chs:dc:dd:db:de:imc:imd:imb:ime:tc:td:tb:te:stc:std:stb:ste:capc:capd:capb:cape:t) (6,(Estado m lj disp),e) = Pictures $ [map] 
desenhaEstado (ab:men:map:civ:x:rD:rI:rT:rS:rC:v3:v2:v1:v0:t3:t2:t1:t0:ls3:ls2:ls1:ls0:y:d:vz:cd:ci:ct:cs:ld:li:lt:ls:chd:chi:cht:chs:dc:dd:db:de:imc:imd:imb:ime:tc:td:tb:te:stc:std:stb:ste:capc:capd:capb:cape:t) (7,(Estado m lj disp),e) = Pictures $ [civ] ++ (retangulosVida (600) 400 (rC:rI:[])) ++ (choques 600 400 (t3:t2:t1:t0:[]) lj) ++ (laser 750 405 (ls3:ls2:ls1:ls0:[]) lj) ++ (vida 680 400 (v3:v2:v1:v0:[]) lj) ++ (desenhoMapa vz d y (contadorColunas m) m (concat m) (-300) 360) ++ (desenhaJogadores lj (-300) 360 (capc:capd:capb:cape:imc:imd:imb:ime:[])) ++ (desenhaDisparos m lj disp (-300) 360 (ct:cd:[]) (lt:ld:[]) (cht:chd:[]))
desenhaEstado (ab:men:map:civ:x:rD:rI:rT:rS:rC:v3:v2:v1:v0:t3:t2:t1:t0:ls3:ls2:ls1:ls0:y:d:vz:cd:ci:ct:cs:ld:li:lt:ls:chd:chi:cht:chs:dc:dd:db:de:imc:imd:imb:ime:tc:td:tb:te:stc:std:stb:ste:capc:capd:capb:cape:t) (8,(Estado m lj disp),e) = Pictures $ [civ] ++ (retangulosVida (600) 400 (rC:rI:[])) ++ (choques 600 400 (t3:t2:t1:t0:[]) lj) ++ (laser 750 405 (ls3:ls2:ls1:ls0:[]) lj) ++ (vida 680 400 (v3:v2:v1:v0:[]) lj) ++ (desenhoMapa vz d y (contadorColunas m) m (concat m) (-300) 360) ++ (desenhaJogadores lj (-300) 360 (capc:capd:capb:cape:imc:imd:imb:ime:[])) ++ (desenhaDisparos m lj disp (-300) 360 (ct:cd:[]) (lt:ld:[]) (cht:chd:[]))
desenhaEstado (ab:men:map:civ:x:rD:rI:rT:rS:rC:v3:v2:v1:v0:t3:t2:t1:t0:ls3:ls2:ls1:ls0:y:d:vz:cd:ci:ct:cs:ld:li:lt:ls:chd:chi:cht:chs:dc:dd:db:de:imc:imd:imb:ime:tc:td:tb:te:stc:std:stb:ste:capc:capd:capb:cape:t) (9,(Estado m lj disp),e) = Pictures $ [civ] ++ (retangulosVida (600) 400 (rC:rI:[])) ++ (choques 600 400 (t3:t2:t1:t0:[]) lj) ++ (laser 750 405 (ls3:ls2:ls1:ls0:[]) lj) ++ (vida 680 400 (v3:v2:v1:v0:[]) lj) ++ (desenhoMapa vz d y (contadorColunas m) m (concat m) (-300) 360) ++ (desenhaJogadores lj (-300) 360 (capc:capd:capb:cape:imc:imd:imb:ime:[])) ++ (desenhaDisparos m lj disp (-300) 360 (ct:cd:[]) (lt:ld:[]) (cht:chd:[]))




-- * Estado Gloss

type EstadoGloss = (Int , Estado, [Key])


estadoGlossInicial ::  EstadoGloss
estadoGlossInicial = (0, estadoneutro,[])

estadomenu = (1,estadoneutro,[])
estadomapa= (2,estadoneutro,[])
estado1m = (3,estado1,[])
estado2m = (4,estado2,[])
estado3m = (5,estado3,[])
estadomapaS = (6,estadoneutro,[]) 
estado1s =(7,estado_1,[])
estado2s = (8,estado_2,[])
estado3s = (9,estado_3,[])


-- | Função que reage aos eventos que o utilizador possa criar 
reageEventoGloss :: Event -- ^ Evento criado pelo o utilizador
                 -> EstadoGloss -- ^ Estado Inicial
                 -> EstadoGloss -- ^ Estado resultante depois do evento
reageEventoGloss  (EventKey (SpecialKey KeyEnter)  Down _ _ ) (0, estadoneutro,e) = (1,estadoneutro,e)
reageEventoGloss (EventKey (Char 'z')  Down _ _) (1,estadoneutro,e) = (2,estadoneutro,e) 
reageEventoGloss (EventKey (Char 'x')  Down _ _) (2,estadoneutro,e) = (3,estado1,e)
reageEventoGloss (EventKey (Char 'c')  Down _ _) (2,estadoneutro,e) = (4,estado2,e)
reageEventoGloss (EventKey (Char 'v')  Down _ _) (2,estadoneutro,e) = (5,estado3,e)
reageEventoGloss (EventKey (Char 'b')  Down _ _) (3,estado1,e) = (2,estadoneutro,e)
reageEventoGloss (EventKey (Char 'b')  Down _ _) (4,estado2,e) = (2,estadoneutro,e)
reageEventoGloss (EventKey (Char 'b')  Down _ _) (5,estado3,e) = (2,estadoneutro,e)
reageEventoGloss (EventKey (Char 'b')  Down _ _) (2,estadoneutro,e) = (1,estadoneutro,e)

reageEventoGloss (EventKey (Char 'x')  Down _ _) (1,estadoneutro,e) = (6,estadoneutro,e)
reageEventoGloss (EventKey (Char 'x')  Down _ _) (6,estadoneutro,e) = (7,estado_1,e)
reageEventoGloss (EventKey (Char 'c')  Down _ _) (6,estadoneutro,e) = (8,estado_2,e)
reageEventoGloss (EventKey (Char 'v')  Down _ _) (6,estadoneutro,e) = (9,estado_3,e)
reageEventoGloss (EventKey (Char 'v')  Down _ _) (2,estadoneutro,e) = (5,estado3,e)
reageEventoGloss (EventKey (Char 'b')  Down _ _) (7,estado_1,e) = (6,estadoneutro,e)
reageEventoGloss (EventKey (Char 'b')  Down _ _) (8,estado_2,e) = (6,estadoneutro,e)
reageEventoGloss (EventKey (Char 'b')  Down _ _) (9,estado_2,e) = (6,estadoneutro,e)
reageEventoGloss (EventKey (Char 'b')  Down _ _) (6,estadoneutro,e) = (1,estadoneutro,e)

-- 1º jogador 
reageEventoGloss (EventKey (SpecialKey KeyUp)    Down _ _) (n,(Estado m j d),e) = (n,(Estado m j d), (SpecialKey KeyUp):e) 
reageEventoGloss (EventKey (SpecialKey KeyUp)    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d),(removeKey (SpecialKey KeyUp) e))
reageEventoGloss (EventKey (SpecialKey KeyDown)  Down _ _) (n,(Estado m j d),e) =(n,(Estado m j d),(SpecialKey KeyDown):e)
reageEventoGloss (EventKey (SpecialKey KeyDown)    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d),(removeKey (SpecialKey KeyDown) e))
reageEventoGloss (EventKey (SpecialKey KeyLeft)  Down _ _) (n,(Estado m j d),e) = (n,(Estado m j d),(SpecialKey KeyLeft):e)
reageEventoGloss (EventKey (SpecialKey KeyLeft)    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d),(removeKey (SpecialKey KeyLeft) e))
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (n,(Estado m j d),e) = (n,(Estado m j d),(SpecialKey KeyRight):e)
reageEventoGloss (EventKey (SpecialKey KeyRight)    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d),(removeKey (SpecialKey KeyRight) e))

reageEventoGloss (EventKey (Char ',')  Down _ _ ) (n,(Estado m j d),e) = (n,(jogada 0 (Dispara Canhao) (Estado m j d)),e)
reageEventoGloss (EventKey (Char '.')  Down _ _ ) (n,(Estado m j d),e) = (n,(jogada 0 (Dispara Laser) (Estado m j d)),e)
reageEventoGloss (EventKey (Char 'm')  Down _ _ ) (n,(Estado m j d),e) = (n,(jogada 0 (Dispara Choque) (Estado m j d)),e)

-- 2º Jogador
reageEventoGloss (EventKey (Char 'w')    Down _ _) (n,(Estado m j d),e) =(n,(Estado m j d), ((Char 'w') :e)) 
reageEventoGloss (EventKey (Char 'w')    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d), (removeKey (Char 'w') e))
reageEventoGloss (EventKey (Char 's')    Down _ _) (n,(Estado m j d),e) =(n,(Estado m j d), ((Char 's') :e))
reageEventoGloss (EventKey (Char 's')    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d), (removeKey (Char 's') e))
reageEventoGloss (EventKey (Char 'a')    Down _ _) (n,(Estado m j d),e) =(n,(Estado m j d), ((Char 'a') :e))
reageEventoGloss (EventKey (Char 'a')    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d), (removeKey (Char 'a') e))
reageEventoGloss (EventKey (Char 'd')    Down _ _) (n,(Estado m j d),e) =(n,(Estado m j d), ((Char 'd') :e))
reageEventoGloss (EventKey (Char 'd')    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d), (removeKey (Char 'd') e))

reageEventoGloss (EventKey (Char '1')  Down _ _ ) (n,(Estado m j d),e) = (n,(jogada 1 (Dispara Canhao) (Estado m j d)),e)
reageEventoGloss (EventKey (Char '2')  Down _ _ ) (n,(Estado m j d),e) = (n,(jogada 1 (Dispara Laser) (Estado m j d)),e)
reageEventoGloss (EventKey (Char '3')  Down _ _ ) (n,(Estado m j d),e) = (n,(jogada 1 (Dispara Choque) (Estado m j d)),e)
 
--3 Jogador 
reageEventoGloss (EventKey (Char 't')    Down _ _) (n,(Estado m j d),e) =(n,(Estado m j d), ((Char 't') :e)) 
reageEventoGloss (EventKey (Char 't')    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d), (removeKey (Char 't') e))
reageEventoGloss (EventKey (Char 'g')    Down _ _) (n,(Estado m j d),e) =(n,(Estado m j d), ((Char 'g') :e))
reageEventoGloss (EventKey (Char 'g')    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d), (removeKey (Char 'g') e))
reageEventoGloss (EventKey (Char 'f')    Down _ _) (n,(Estado m j d),e) =(n,(Estado m j d), ((Char 'f') :e))
reageEventoGloss (EventKey (Char 'f')    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d), (removeKey (Char 'f') e))
reageEventoGloss (EventKey (Char 'h')    Down _ _) (n,(Estado m j d),e) =(n,(Estado m j d), ((Char 'h') :e))
reageEventoGloss (EventKey (Char 'h')    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d), (removeKey (Char 'h') e))

reageEventoGloss (EventKey (Char '4')  Down _ _ ) (n,(Estado m j d),e) = (n,(jogada 2 (Dispara Canhao) (Estado m j d)),e)
reageEventoGloss (EventKey (Char '5')  Down _ _ ) (n,(Estado m j d),e) = (n,(jogada 2 (Dispara Laser) (Estado m j d)),e)
reageEventoGloss (EventKey (Char '6')  Down _ _ ) (n,(Estado m j d),e) = (n,(jogada 2 (Dispara Choque) (Estado m j d)),e)

--4º Jogador

reageEventoGloss (EventKey (Char 'i')    Down _ _) (n,(Estado m j d),e) =(n,(Estado m j d), ((Char 'i') :e)) 
reageEventoGloss (EventKey (Char 'i')    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d), (removeKey (Char 'i') e))
reageEventoGloss (EventKey (Char 'k')    Down _ _) (n,(Estado m j d),e) =(n,(Estado m j d), ((Char 'k') :e))
reageEventoGloss (EventKey (Char 'k')    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d), (removeKey (Char 'k') e))
reageEventoGloss (EventKey (Char 'j')    Down _ _) (n,(Estado m j d),e) =(n,(Estado m j d), ((Char 'j') :e))
reageEventoGloss (EventKey (Char 'j')    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d), (removeKey (Char 'j') e))
reageEventoGloss (EventKey (Char 'l')    Down _ _) (n,(Estado m j d),e) =(n,(Estado m j d), ((Char 'l') :e))
reageEventoGloss (EventKey (Char 'l')    Up _ _) (n,(Estado m j d),e) = (n,(Estado m j d), (removeKey (Char 'l') e))


reageEventoGloss (EventKey (Char '7')  Down _ _ ) (n,(Estado m j d),e) = (n,(jogada 3 (Dispara Canhao) (Estado m j d)),e)
reageEventoGloss (EventKey (Char '8')  Down _ _ ) (n,(Estado m j d),e) = (n,(jogada 3 (Dispara Laser) (Estado m j d)),e)
reageEventoGloss (EventKey (Char '9')  Down _ _ ) (n,(Estado m j d),e) = (n,(jogada 3 (Dispara Choque) (Estado m j d)),e) 


reageEventoGloss _ e = e

-- | Função que remove o priemiro elemento de uma lista de keys
removeKey :: Key -- ^ Recebe uma key 
          -> [Key] -- ^ Recebe uma lista de Keys
          -> [Key] -- ^ Lista de Keys sem o primeiro elemento
removeKey a (h:t) | a == h = t
                  | otherwise = h : removeKey a t


-- | Função que com a função aplicaKey transforma uma lista de keys num estado 
aplicaListaKey :: [Key] -- ^ Recebe lista de keys 
               -> Estado -- ^ Recebe um estado inicial
               -> Estado -- ^ Estado resultante da alteração efetuada pelas várias keys
aplicaListaKey [] e = e
aplicaListaKey (h:t) e = aplicaListaKey t (aplicakey h e) 

-- | Função que aplica uma determinada Key a um estado 
aplicakey :: Key -- ^ Key Pressionada 
          -> Estado -- ^ Estado atual 
          -> Estado -- ^ Estado alterado pelo evento 
aplicakey (SpecialKey KeyUp) (Estado m j d)    = if jogadorExiste 0 j then (jogada 0 (Movimenta C) (Estado m j d)) else (Estado m j d) 
aplicakey (SpecialKey KeyDown) (Estado m j d)  = if jogadorExiste 0 j then (jogada 0 (Movimenta B) (Estado m j d)) else (Estado m j d) 
aplicakey (SpecialKey KeyLeft) (Estado m j d)  = if jogadorExiste 0 j then (jogada 0 (Movimenta E) (Estado m j d)) else (Estado m j d) 
aplicakey (SpecialKey KeyRight) (Estado m j d) = if jogadorExiste 0 j then (jogada 0 (Movimenta D) (Estado m j d)) else (Estado m j d) 
aplicakey (Char 'w') (Estado m j d) = if jogadorExiste 1 j then (jogada 1 (Movimenta C) (Estado m j d)) else (Estado m j d)
aplicakey (Char 's') (Estado m j d) = if jogadorExiste 1 j then (jogada 1 (Movimenta B) (Estado m j d)) else (Estado m j d)
aplicakey (Char 'a') (Estado m j d) = if jogadorExiste 1 j then (jogada 1 (Movimenta E) (Estado m j d)) else (Estado m j d)
aplicakey (Char 'd') (Estado m j d) = if jogadorExiste 1 j then (jogada 1 (Movimenta D) (Estado m j d)) else (Estado m j d)
aplicakey (Char 't') (Estado m j d) = if jogadorExiste 2 j then (jogada 2 (Movimenta C) (Estado m j d)) else (Estado m j d)
aplicakey (Char 'g') (Estado m j d) = if jogadorExiste 2 j then (jogada 2 (Movimenta B) (Estado m j d)) else (Estado m j d)
aplicakey (Char 'f') (Estado m j d) = if jogadorExiste 2 j then (jogada 2 (Movimenta E) (Estado m j d)) else (Estado m j d)
aplicakey (Char 'h') (Estado m j d) = if jogadorExiste 2 j then (jogada 2 (Movimenta D) (Estado m j d)) else (Estado m j d)
aplicakey (Char 'i') (Estado m j d) = if jogadorExiste 3 j then (jogada 3 (Movimenta C) (Estado m j d)) else (Estado m j d)
aplicakey (Char 'k') (Estado m j d) = if jogadorExiste 3 j then (jogada 3 (Movimenta B) (Estado m j d)) else (Estado m j d)
aplicakey (Char 'j') (Estado m j d) = if jogadorExiste 3 j then (jogada 3 (Movimenta E) (Estado m j d)) else (Estado m j d)
aplicakey (Char 'l') (Estado m j d) = if jogadorExiste 3 j then (jogada 3 (Movimenta D) (Estado m j d)) else (Estado m j d)

jogadorExiste :: Int -> [Jogador] -> Bool
jogadorExiste x [] = False
jogadorExiste 0 l = True
jogadorExiste x (h:t) = jogadorExiste (x-1) t

-- | Função que faz passar os ticks e adequa o esatdo conforme os ticks
reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss t (n, e, le) = (n, tick (aplicaListaKey le e), le)

--frame rate
fr :: Int
fr = 10

--controla o tamanho e a posição da janela no ecra
dm :: Display
dm = FullScreen 
