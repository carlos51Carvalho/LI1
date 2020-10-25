{- | Module      : Tarefa3_2018li1g146
     Description : Compressão e descompressao do estado de um jogo (mapa, lista de Jogadores e lista de disparos)
     Copyright   : Carlos Carvalho <a89605@alunos.uminho.pt>
                   Fernando Lopes <a89472@alunos.uminho.pt> 

= Resumo:
Nesta Tarefa era esperado que conseguissemos criar uma função capaz de comprimir um Estado e uma outra que conseguisse descomprimi lo, para o comprimir separamos a informação e criamos várias
funçoes para comprimir cada parte dessa informação separada juntando todas essas auxiliares da comprime numa só, já para o caso da descomprime separamos também a informação com o auxilio de chaves
criadas na comprime de forma a melhor identificar as varias partes da informação e fomos de seguida fazer o processo inverso, conseguindo por fim com que a descomprime (comprime e) = e  


= Introdução :
Nesta Tarefa a ideia era trabalhar a compressão de um estado de jogo e sucessivamente na sua descompressão, procurando obter um estado exatamente igual ao que foi posteriormente comprimido , 
de forma a facilitar com ajuda destas duas ferramentas (compressão e descompressão), o fácil manuseamento de muita informação ao mesmo tempo.
Fazendo-se assim notar com maiores dificuldades a descompressão, já que tinhamos que voltar exatante ao estado incial (não comprimido) ou seja,

descomprime (comprime e ) = e  sendo e um estado 


= Objetivos e estratégias:

== Compressão:   

Numa fase inicial começámos por procurar uma forma de conseguir comprimir a variada informção dada pelo Estado, 
para isso resolvemos então separar esta informação e comprimi-la em diversas funções distintas de modo a com o minimo de texto conseguirmos dar o máximo de informção, 
assim sendo criamos uma função para comprimir o mapa 'mapaToString' que vai transformar cada peça do mapa num caracter através da função 'interpretaPeca', 
vamos com uma outra função a 'passaString' obter uma lista lista de caracters variados, onde caso hajam tres caracteres consecutivos iguais é utilizada uma outra denotação,caso não haja
mantém se então a definida na 'interpretaPeca' sendo esta organizada numa lista,
por exemplo uma primeira linha de um mapa com 8 Blocos Indestrutiveis seria representada pela comprime da seguinte forma : "IIii".
Continuando na tarefa de comprimir o estado passamos para a compressão dos Jogadores, aqui começamos por procurar comprimir apenas um Jogador, desta forma com o intuito de nos facilitar a leitura
e postriormente a descompressão, sendo o Jogador algo que recebe bastante informação, separar com caracteres "chave" os seus elementos comprimidos, visivel na função 'jogadorToString',
obtendo na compressaão de um jogador por exemplo (Jogador (3,4) C 2 1 1) uma string do tipo "3,4.C.2.1.1", conseguindo isto, avançamos para a compressão de uma lista de Jogadores,
onde para isso utilizamos a função criada anteriormente e para disntiguir os vários jogadores usamos um outro carcter chave "/" no meio da informação comprimida de cada um.
Por fim fomos comprimir os disparos onde utilizámos as mesmas estratégias que usamos para os Jogadores, começamos por comprimir apenas um e só um disparo, mas tendo sempre em atenção
que existiam vários tipos de disparos, tendo por isso que arranjar uma maneira de os identificar na compressão (usamos o "c" o "l" e o "h" de forma a identifar os diferentes tipos),
de seguida fomos então tal como na compressão dos jogadores criar uma chave para separar os vários disparos, para nos facilitar a descompressão.
Por ultimo fomos motar com ajuda de todas estas auxiliares a função comprime fazendo com que esta conseguisse comprimir o Estado e toda a sua informação.


== Descompressão:

Na função descomprime fomos mais uma vez separar a informação, para isso aproveitamos as chaves que criamos na comprime, e uma vez separada fomos trabalhar a informação de modo
a transformar esta informação novamente num Estado normal. Seguimos então o processo inverso, na descompressão do mapa começámos por redefinir a que cada carater correspondia em termos de blocos ('descomprimeElemento')
de seguida fomos aplicar essa função a uma linha completa e sucessivamente a um mapa tendo sempre em atenção onde começava e acabava cada linha. De seguida na descompressão dos jogadores fomos também
pelo processo inverso descomprimindo primeiro apenas um jogador e depois com ajuda das chaves que utilizamos na compressão descomprimir uma lista de Jogadores, da mesma forma e tendo sempre em atenção a variadade fizemos a mesma coisa para os disparos
pela mesma estrategia, isto foi culminar na função descomprime que foi capaz de reproduzir o Estado inicial antes da compressão. 

= Conclusão:

Por concluir, conseguimos concluir esta tarefa, fazendo ela o que lhe era esperado, sobre a tarefa em si o processo da descompressão mostrou se mais complicado já que este teve de ser de alguma forma
idealizado na parte da compressão para criarmos ferramentas como as chaves separadoras que nos ajudaram a descomprimir    
     -}






-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2018li1g146 where

import LI11819
import Tarefa0_2018li1g146
import Tarefa1_2018li1g146
import Tarefa2_2018li1g146

-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Estado'.
testesT3 :: [Estado]
testesT3 = [e1,e2,e3,e4]


mapa1 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],
        [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]

mapa_2 = [[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Bloco Destrutivel,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Vazia,Bloco Indestrutivel],[Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]]

disparos = [(DisparoCanhao 0 (3,2) D), (DisparoCanhao 0 (6,2) E), (DisparoCanhao 0 (8,1) B), (DisparoCanhao 0 (6,2) D),(DisparoLaser 2 (2,5) B), (DisparoChoque 3 5)]
dsparos0 = []

e1 = (Estado mapa1 [jogador0, jogador1, jogador2] disparos )
e2 = (Estado mapa_2 [jogador0] dsparos0 )
e3 = (Estado mapa_2 [jogador2,jogador0,jogador1,jogador4] dsparos0 )
e4 = (Estado mapa1 [jogador1,jogador0] disparos)


-- * Funções principais da Tarefa 3.

-- | Comprime um 'Estado' para formato textual.
--
-- __NB:__ A função 'show' representa um 'Estado' num formato textual facilmente legível mas extenso.
--
-- __NB:__ Uma boa solução deve representar o 'Estado' dado no mínimo número de caracteres possível.
comprime :: Estado -- ^ Recebe um 'Estado'.
         -> String -- ^ Transforma o 'Estado' em 'String'.
comprime (Estado m j d) = (mapaToString m) ++ "_" ++ (comprimeJogador j) ++ "_" ++ (disparosToString d)


-- * Funções que comprimem o 'Mapa'

-- | Comprime um 'Mapa' em formato textual
mapaToString :: Mapa   -- ^ Recebe um 'Mapa'.
             -> String -- ^ Passa o 'Mapa' a 'String'.
mapaToString [] = "n"
mapaToString (h:t) = passaString h ++ (mapaToString t)
                 

-- | Comprime uma linha do 'Mapa' em formato textual e caso existao seuquencias de 3 'Peca' iguais comprime a apenas um carater.  
passaString ::[Peca]  -- ^ Recebe uma lista de 'Peca'.
            -> String -- ^ Transforma 'Peca' em 'String'.
passaString [] = "n"
passaString (h1:h2:h3:y)|h1==h2 && h2==h3 && (intepretaPeca h1) == "v" = "V" ++ (passaString y)
                        |h1==h2 && h2==h3 && (intepretaPeca h1) == "i" = "I" ++ (passaString y)
                        |h1==h2 && h2==h3 && (intepretaPeca h1) == "d" = "D" ++ (passaString y) 
passaString (h:y) = intepretaPeca h ++ passaString y



-- | Comprime uma 'Peca' em formato textual.
intepretaPeca ::Peca    -- ^ Recebe uma 'Peca'.
              -> String -- ^ Transforma a 'Peca' em 'String'.
intepretaPeca h | h ==(Bloco Indestrutivel) = "i" 
                | h == Vazia = "v" 
                | otherwise = "d" 


-- * Funções que comprimem o 'Jogador'

-- | Comprime um 'Jogador' em formação textual.
jogadorToString :: Jogador -- ^ Reecebe um 'Jogador'.
                -> String  -- ^ Transforma o 'Jogador' em 'String'.
jogadorToString (Jogador (l,c) d v ls ch) = show l ++ "," ++ show c ++ "." ++ show d ++ "." ++ show v ++ "." ++ show ls ++ "." ++ show ch 


-- | Comprime uma lista de 'Jogador' em formato textual.
comprimeJogador:: [Jogador] -- ^ Recebe uma lista de 'Jogador' 
               -> String    -- ^ Transforma os 'Jogador' em 'String'
comprimeJogador [] = "!"
comprimeJogador (h:t) = jogadorToString h ++ "/" ++ (comprimeJogador t) 


-- * Funções que comprimem o 'Disparo'


-- | Comprime um 'Disparo' em formato textual.
disparoToString:: Disparo -- ^ Recebe um 'Disparo'
               -> String  -- ^ Produz uma 'String'
disparoToString (DisparoCanhao n (l,c) d ) = "c"++ show n ++ "."++ show l ++ "," ++ show c++ "." ++ show d 
disparoToString (DisparoLaser n (l,c) d ) = "l" ++ show n ++ "."++ show l ++ "," ++ show c++ "." ++ show d
disparoToString (DisparoChoque n t) = "h" ++ show n ++ "."++ show t


-- | Comprime uma lista de 'Disparo' em formato textual.
disparosToString:: [Disparo] -- ^ Recebe uma lista de 'Disparo' 
                -> String    -- ^ Produz uma String
disparosToString [] = "#"
disparosToString (h:t) = disparoToString h ++ "|" ++ (disparosToString t) 





-- | Descomprime um 'Estado' no formato textual utilizado pela função 'comprime'.
--
-- __NB:__ A função 'comprime' é válida de for possível recuperar o 'Estado' utilizando a função 'descomprime', i.e.:
--
-- prop> descomprime . comprime = id
--
-- __NB:__ Esta propriedade é particularmente válida para a solução pré-definida:
--
-- prop> read . show = id
descomprime :: String -- ^ Recebe uma 'String'
            -> Estado -- ^ Produz um Estado
descomprime s = let m = takeWhile (/='_') s
                    j = takeWhile (/='_') (tail (dropWhile (/='_') s))
                    d = tail (dropWhile (/='_') (tail (dropWhile (/='_') s )))
                in (Estado (descomprimeMapa m) (descomprimeListaJogadores j) (descomprimeListaDisparos d) )


-- * Funções que descomprimem o 'Mapa'               

-- | Descomprime um carater formado pela função 'comprime' passando-o para 'Peca'.
descomprimeElemento :: Char -- ^ Recebe um carater ('Char').
                    -> [Peca] -- ^ Produz uma 'Peca'.
descomprimeElemento x | x == 'i' = [Bloco Indestrutivel]
                      | x == 'd' = [Bloco Destrutivel]
                      | x == 'v' = [Vazia]
                      | x == 'I' = [Bloco Indestrutivel,Bloco Indestrutivel,Bloco Indestrutivel]
                      | x == 'D' = [Bloco Destrutivel,Bloco Destrutivel, Bloco Destrutivel]
                      | otherwise = [Vazia,Vazia,Vazia]



-- | Descomprime uma 'String' numa lista de 'Peca'.
descomprimeLinhaMapa :: String -- ^ Recebe uma 'String'.
                     -> [Peca] -- ^ Produz a lista de 'Peca' respetiva. 
descomprimeLinhaMapa [] = []
descomprimeLinhaMapa ('n':t) = [] 
descomprimeLinhaMapa (h:t) = (descomprimeElemento h) ++ (descomprimeLinhaMapa t)


-- | Descomprime uma 'String' num 'Mapa' correspondente.
descomprimeMapa :: String -- ^ Recebe uma 'String' .
                -> Mapa   -- ^ Produz um 'Mapa' correspondente.
descomprimeMapa "" = [] 
descomprimeMapa ('n':t) = descomprimeMapa t
descomprimeMapa s = (descomprimeLinhaMapa s) : (descomprimeMapa (drop 1 (dropWhile (/='n') s)))
              

-- * Funções que descomprimem o 'Jogador'

-- | Descomprime uma 'String' num 'Jogador'
descomprimeJogador::String   -- ^ Recebe uma 'String'. 
                  -> Jogador -- ^ Produz um 'Jogador' correspondente.
descomprimeJogador (h:t) = let l = read (takeWhile (/=',') (h:t))::Int
                               c = read (tail (dropWhile (/=',') (takeWhile (/='.') (h:t))))::Int  
                               d = read [head (auxiliarJogador t)]::Direcao 
                               v = read (takeWhile (/='.') (auxiliarJogador (auxiliarJogador t)))::Int 
                               ls= read (takeWhile (/='.') (auxiliarJogador(auxiliarJogador(auxiliarJogador t))))::Int 
                               ch= read (takeWhile (/='/') (auxiliarJogador(auxiliarJogador(auxiliarJogador(auxiliarJogador t)))))::Int
                           in Jogador (l,c) d v ls ch  
 

-- | Subtrai á 'String' os elementos até encontrar um '.'.
auxiliarJogador::String  -- ^ Recebe uma 'String' inicial.
               -> String -- ^ 'String' inicial alterada. 
auxiliarJogador "" = ""
auxiliarJogador j = tail (dropWhile (/='.') j)


-- | Descomprime uma 'String' numa lista de 'Jogador' correspondente.
descomprimeListaJogadores :: String    -- ^ Recebe uma 'String'
                          -> [Jogador] -- ^ Produz uma lista de 'Jogador' correspondente 
descomprimeListaJogadores "" = []
descomprimeListaJogadores ('!':_) = []
descomprimeListaJogadores ('/':t) = descomprimeListaJogadores t
descomprimeListaJogadores (h:t) = (descomprimeJogador (h:t)) : (descomprimeListaJogadores (dropWhile (/='/') t))




-- * Funções que descomprimem o 'Disparo'



-- | Descomprime uma 'String' num 'Disparo' correspondente.
descomprimeDisparo :: String  -- ^ Recebe uma 'String'. 
                   -> Disparo -- ^ Produz o 'Disparo' correspondente.
descomprimeDisparo ('c':t) = let j = read [head t] ::Int
                                 l = read (tail (takeWhile (/=',') (dropWhile (/='.') t))) ::Int
                                 c = read (tail (takeWhile (/='.') (dropWhile (/=',') t))) ::Int
                                 d = read [head (tail (dropWhile (/='.') (tail (dropWhile (/='.') t))))] ::Direcao
                             in DisparoCanhao j (l,c) d    
descomprimeDisparo ('l':t) = let j = read [head t] ::Int
                                 l = read (tail (takeWhile (/=',') (dropWhile (/='.') t))) ::Int
                                 c = read (tail (takeWhile (/='.') (dropWhile (/=',') t))) ::Int
                                 d = read [head (tail (dropWhile (/='.') (tail (dropWhile (/='.') t))))] ::Direcao
                             in DisparoLaser j (l,c) d
descomprimeDisparo ('h':t) = let j = read [head t] ::Int
                                 tik = read [head (tail (dropWhile (/='.') t))] ::Ticks
                             in DisparoChoque j tik


-- | Descomprime duma 'String' uma lista de 'Disparo' euqivalente.
descomprimeListaDisparos::String -- ^ Recebe uma 'String' 
                        ->[Disparo] -- ^ Produz uma lista de 'Disparo' correspondente. 
descomprimeListaDisparos ""= []
descomprimeListaDisparos ('#':t)= []
descomprimeListaDisparos ('|':t) = descomprimeListaDisparos t
descomprimeListaDisparos (h:t) = descomprimeDisparo (h:t) : (descomprimeListaDisparos (dropWhile (/='|') t))                                                        


