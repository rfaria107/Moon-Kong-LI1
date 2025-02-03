{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Rui Alexandre Oliveira Faria <a106899@alunos.uminho.pt>
              Octávio Pita Henriques <a104277@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where
import LI12324
{-| A função colisoesParede verifica se um personagem se encontra em cima de um bloco do tipo Plataforma
  
== Exemplos de utilização:

>>> colisoesParede mapa1 p1
False
      where (posicao p1) = (2.5,8)
>>> colisoesParede mapa p1
True
            where (posicao p1) = (1,1)
-}
colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede m1@(Mapa _ _ mblocos) p1 = colisoesJogadorParede m1 p1 && colisaoBloco p1 (hitboxPlataformas (posicaoBloco mblocos))

-- | Função que verifica se um personagem se encontra dentro dos limites do mapa
colisoesJogadorParede :: Mapa -> Personagem -> Bool
colisoesJogadorParede (Mapa _ _ (row:_)) p = 
  let ((leftX, _), (rightX, _)) = defineHitbox p
      mapWidth = fromIntegral (length row)
  in leftX < 1 || rightX > mapWidth -1
    
-- colisões entre personagens
 -- | Função que, dada uma personagem, define a sua hitbox
defineHitbox :: Personagem -> Hitbox 
defineHitbox p = (ci,cs) -- ^ Hitbox é definida pelo canto inferior esquerdo e canto superior direito
                    where ci = (fst (posicao p) - fst (tamanho p)/2 , snd (posicao p) - snd (tamanho p)/2)
                          cs = (fst (posicao p) + fst (tamanho p)/2 , snd (posicao p) + (snd (tamanho p)/2))

-- | Função que verifica se duas hitboxes se sobrepõem
colisaoHitbox :: Hitbox -> Hitbox -> Bool 
colisaoHitbox ((x1,y1),(w1,z1)) ((x2,y2),(w2,z2))
                                                      |((x1 >= x2 && x1 <= w2) || (w1 >= x2 && w1 <= w2)) && ((y1 >= y2 && y1 <= z2) || (z1 >= y2 && z1 <= z2)) = True
                                                      |otherwise = False

-- | utiliza as duas funções anteriores para verificar colisões entre personagens
colisoesPersonagens :: Personagem -> Personagem -> Bool 
colisoesPersonagens p1 p2 = colisaoHitbox h1 h2
                                where h1 = defineHitbox p1
                                      h2 = defineHitbox p2

-- colisões entre personagens e plataformas

-- | atribui a cada bloco da matriz uma posição
posicaoBloco :: [[Bloco]] -> [(Posicao,Bloco)]
posicaoBloco mblocos = [((fromIntegral x, fromIntegral y),bloco) | (y,linhaBloco) <- zip [0..] mblocos, (x,bloco) <- zip [0..] linhaBloco]

-- | calcula a hitbox de uma plataforma
hitboxPlat :: (Posicao,Bloco) -> Hitbox 
hitboxPlat ((x,y),Plataforma) = ((x-0.5,y-0.5),(x+0.5,y+0.5))
hitboxPlat (_,_) = ((0,0),(0,0))

-- | calcula a lista das hitboxes de todas as plataforma do mapa
hitboxPlataformas :: [(Posicao,Bloco)]-> [Hitbox] 
hitboxPlataformas = map hitboxPlat

colisaoBloco :: Personagem -> [Hitbox] -> Bool
colisaoBloco p1@(Personagem _ _ (x,y) _ _ _ _ _ _ _) plataformas = any (==True) (map (colisaoHitbox (defineHitbox p1)) plataformas)

-- | colisões úteis para outras funções
-- | escolhe o bloco abaixo da personagem (útil para colisões com alçapão)
escolheBloco :: Personagem -> Mapa -> Bloco 
escolheBloco p1@(Personagem _ _ (x,y) _ (l,a) _ _ _ _ _) (Mapa _ _ mblocos) = (mblocos !! floor (y+0.2)) !! floor x

-- | verifica se a persongem está em cima de uma plataforma
colisaoBlocoPersonagem :: Personagem -> Bloco -> Bool 
colisaoBlocoPersonagem p1 b = colisaoHitbox (defineHitbox p1) (hitboxPlat ((fst (posicao p1),snd (posicao p1)),b)) 

-- | função que, após receber uma posição e uma matriz retorna qual o tipo de bloco nesta posição da matriz
escolheBlocoPos :: Posicao -> [[Bloco]] -> Bloco 
escolheBlocoPos  (x,y) matriz = (matriz !! floor y) !! floor x

-- | verifica se o personagem está em escada
colisaoPersonagemEscada :: Personagem -> Jogo -> Bool 
colisaoPersonagemEscada p1 j1@(Jogo (Mapa _ _ mblocos) _ _ _)
                                                |escolheBlocoPos (posicao p1) mblocos == Escada || escolheBlocoPos ( fst (posicao p1), snd (posicao p1) +1) mblocos == Escada = True
                                                |otherwise = False
-- | versão com mapa em vez de jogo
colisaoPersonagemEscada2 :: Personagem -> Mapa -> Bool
colisaoPersonagemEscada2 p1 (Mapa _ _ mblocos)
                                                |escolheBlocoPos (posicao p1) mblocos == Escada || escolheBlocoPos ( fst (posicao p1), snd (posicao p1) +1) mblocos == Escada = True
                                                |otherwise = False
