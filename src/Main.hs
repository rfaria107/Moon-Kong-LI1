module Main where
import LI12324 
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game 
import Mapa
import Data.Maybe (fromJust)
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import GHC.Float (float2Double, double2Float, float2Int, int2Double)
import System.Random

type Images = [Picture]
type Imagens = [(String,Picture)]
type EstadoMapa = (Jogo, Imagens)
data World = 
        World {
                menu :: Menu,
                jogo :: Jogo,
                gameover :: Menu,
                vitoria :: Menu,
                menuatual :: String
                }

data Menu = Menu Int | GameOver Int | Vitoria Int

hitboxLarguraj :: Float
hitboxLarguraj = 96

hitboxAlturaj :: Float
hitboxAlturaj = 96

hitboxLarguraO :: Float
hitboxLarguraO =  60

hitboxAlturaO :: Float
hitboxAlturaO = 100

altura :: Float
altura =  1080

comprimento :: Float 
comprimento = -1920

--largura das peças do mapa
l :: Float
l = 96

--Desenhar o Menu

desenhaMenu :: Menu -> Imagens -> Picture
desenhaMenu (Menu menu1) images = Scale 1 1 $ Translate 0 0 $ fromJust $ lookup "Menu" images 

desenhawin :: Menu -> Imagens -> Picture
desenhawin (Menu menu1) images = Translate 0 0 $ fromJust $ lookup "FundoVitoria" images 

desenhalose :: Menu -> Imagens -> Picture
desenhalose (Menu menu1) images = Translate 0 0 $ fromJust $ lookup "FundoGameOver" images 

--não utilizado
janela :: Display
janela = InWindow "Moon Kong" (1920,1080) (0,0)

fr :: Int
fr = 60

--Menus e jogos utilizado (os menus são apenas inteiros que servem apenas para indicar o que desenhar no ecrã, essa seleção é feita pela string "menuatual")

defineMenu1 :: Menu
defineMenu1 = Menu 1

menuvitoria1 :: Menu
menuvitoria1 = Menu 2

menugameover1 :: Menu
menugameover1 = Menu 3

definejogo1 :: Jogo
definejogo1 = Jogo m i c j
                where   m = mapa1
                        i = [inimigo1,inimigo2,inimigo3]
                        c = [(Moeda,(15,5)),(Martelo,(17,8.5))]
                        j = jogador1

mundo1 :: World
mundo1 = World defineMenu1 definejogo1 menugameover1 menuvitoria1 "nomenu"

xcentroMatriz = 10 
ycentroMatriz = 5

traduzPosicaoX :: Double -> Float -- traduz uma posicao da lógica da matriz para esta ser utilizada no gloss
traduzPosicaoX x = double2Float (x - fromIntegral xcentroMatriz) * (1920 / 20)

traduzPosicaoY :: Double -> Float -- traduz uma posicao da lógica da matriz para esta ser utilizada no gloss
traduzPosicaoY y = - double2Float (y - fromIntegral ycentroMatriz) * (1080 / 10)

getMapa :: Jogo -> Mapa
getMapa j = mapa j

estadoInicial :: Imagens -> World
estadoInicial imagens = World defineMenu1 definejogo1 menuvitoria1 menugameover1 "nomenu"


reageEvento :: Event -> World -> World
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) mundo1 = mundo1 { menuatual = "jogo"}
reageEvento (EventKey (SpecialKey KeySpace) Down _ _) mundo1 =  mundo1 { jogo = (atualiza (geraAcoes 2 5) (Just Saltar) (jogo mundo1))}
reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) mundo1 = mundo1 { jogo = (atualiza (geraAcoes 1 3)  (Just AndarEsquerda) (jogo mundo1)) }
reageEvento (EventKey (SpecialKey KeyLeft) Up _ _) mundo1 = mundo1 { jogo = (atualiza (geraAcoes 3 8) (Just Parar) (jogo mundo1)) }
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) mundo1 = mundo1 { jogo =  (atualiza (geraAcoes 4 5)  (Just AndarDireita) (jogo mundo1)) }
reageEvento (EventKey (SpecialKey KeyRight) Up _ _) mundo1 = mundo1 { jogo = (atualiza (geraAcoes 6 1)  (Just Parar) (jogo mundo1)) }
--escadas
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (mundo1) = if colisaoPersonagemEscada (jogador (jogo mundo1)) (jogo mundo1) then mundo1 { jogo = (atualiza (geraAcoes 2 3)  (Just Subir) (jogo mundo1)) } else mundo1 {jogo = (jogo mundo1)}
reageEvento (EventKey (SpecialKey KeyUp) Up _ _) (mundo1) =  mundo1 { jogo = (atualiza (geraAcoes 2 3)  (Just Parar) (jogo mundo1))}
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (mundo1) = if colisaoPersonagemEscada (jogador (jogo mundo1)) (jogo mundo1) then mundo1 { jogo = (atualiza (geraAcoes 2 3) (Just Descer) (jogo mundo1)) } else mundo1 {jogo = (jogo mundo1)}
reageEvento (EventKey (SpecialKey KeyDown) Up _ _) (mundo1) = mundo1 { jogo = (atualiza (geraAcoes 2 3)  (Just Parar) (jogo mundo1))}
reageEvento _ mundo1 = mundo1  

vitoriaderrotajogo :: World -> String
vitoriaderrotajogo mundo    |menuatual mundo == "jogo" && colisaoHitbox (defineHitbox (jogador (jogo mundo))) (defineHitboxPos ((mapa (jogo mundo)))) = "vitoria"
                            |menuatual mundo ==  "jogo" && vida (jogador (jogo mundo)) == 0 = "gameover"
                            |otherwise = menuatual mundo
                                    where defineHitboxPos :: Mapa -> Hitbox
                                          defineHitboxPos (Mapa _ pos _) = (ci,cs)
                                                where ci = (fst pos - 0.5, snd pos - 0.5)
                                                      cs = (fst pos + 0.5 , snd pos + 0.5)

reageTempo :: Float -> World -> World
reageTempo dt mundo1 = mundo1 { jogo = movimenta 3 (float2Double dt) (jogo mundo1), menuatual = vitoriaderrotajogo mundo1 }

desenhaJogador :: Imagens -> Personagem -> Picture
desenhaJogador images j@(Personagem vel Jogador (x,y) direcao tamanho _ _ _ _ armado)
    | fst vel >=0 && fst armado == False =Pictures [ Translate (traduzPosicaoX x) (traduzPosicaoY y + 10) $ Scale 0.9 0.9 $ fromJust $ lookup "MarioDireita" images]
    | fst vel <0 && fst armado == False =Pictures [ Translate (traduzPosicaoX x) (traduzPosicaoY y +10) $ Scale 0.9 0.9$ fromJust $ lookup "MarioEsquerda" images]
    | fst vel >=0 && fst armado == True =Pictures [ Translate (traduzPosicaoX x) (traduzPosicaoY y +10) $ Scale 0.9 0.9 $ fromJust $ lookup "ArmadoDireita" images]
    | fst vel <0 && fst armado == True =Pictures [ Translate (traduzPosicaoX x) (traduzPosicaoY y +10) $ Scale 0.9 0.9 $ fromJust $ lookup "ArmadoEsquerda" images]

desenhaObjetivo :: Imagens -> [(Colecionavel, (Double,Double))] -> Picture
desenhaObjetivo _ [] = blank
desenhaObjetivo images (w@(Moeda, (x,y)):t) = Pictures [Translate (traduzPosicaoX x) (traduzPosicaoY y) $ fromJust $ lookup "Moeda" images, desenhaObjetivo images t ]
desenhaObjetivo images (w@(Martelo, (x,y)):t) = Pictures [Translate (traduzPosicaoX x) (traduzPosicaoY y) $ fromJust $ lookup "Martelo" images, desenhaObjetivo images t]

desenhaVitoria :: Imagens -> Posicao -> Picture
desenhaVitoria images (x,y) = Pictures [Translate (traduzPosicaoX x) (traduzPosicaoY y) $ fromJust $ lookup "Peach" images]

desenhaInimigos :: Imagens -> [Personagem] -> Picture 
desenhaInimigos _ [] = blank
desenhaInimigos images (i@(Personagem _ Fantasma (x,y) direcao tamanho _ _ _ _ _):t) =  Pictures [ Translate (traduzPosicaoX x) (traduzPosicaoY y) $ Scale 1 1 $ fromJust $ lookup "Fantasma" images, desenhaInimigos images t]
desenhaInimigos images (i@(Personagem _ MacacoMalvado (x,y) direcao tamanho _ _ _ _ _):t) =  Pictures [ Translate (traduzPosicaoX x) (traduzPosicaoY y) $ Scale 1 1 $ fromJust $ lookup "Macaco" images, desenhaInimigos images t]

defineHitboxJ :: Personagem -> Picture
defineHitboxJ (Personagem _ Jogador (x,y) direcao tamanho _ _ _ _ _) = Color green $ Translate (traduzPosicaoX x) (traduzPosicaoY y) $ rectangleWire hitboxLarguraj hitboxAlturaj

defineHitboxO :: (Colecionavel,(Double,Double)) -> Picture 
defineHitboxO (Moeda,(x,y)) = Color green $ Translate (traduzPosicaoX x) (traduzPosicaoY y) $ rectangleWire hitboxLarguraO hitboxAlturaO

--mapa 

fazMatriz :: [[Bloco]] -> Double -> [[(Bloco,(Double,Double))]]
fazMatriz [] x = []
fazMatriz mapa@((h:t)) y = fazLinha h 0.5 y : fazMatriz t (y+1)

fazLinha :: [Bloco] -> Double -> Double -> [(Bloco,(Double,Double))]
fazLinha [] _ _ = []
fazLinha (bloco:rblocos) x y = (bloco, (x,y)) : fazLinha rblocos (x+1) y

desenhaMapa :: EstadoMapa -> [[(Bloco,(Double,Double))]] -> Picture
desenhaMapa _ [] = blank
desenhaMapa e@((Jogo mapa _ _ _), images) (h:t) = pictures [desenhaLinhaMapa images h, desenhaMapa e t]

desenhaLinhaMapa :: Imagens -> [(Bloco, (Double,Double))] -> Picture
desenhaLinhaMapa _ [] = blank
desenhaLinhaMapa images ((h,(x,y)):t) = case h of 
                Plataforma -> Pictures [Translate (traduzPosicaoX x) (traduzPosicaoY y) $ Scale 1 1 $ fromJust $ lookup "Plataforma" images, desenhaLinhaMapa images t ]
                Escada -> Pictures     [Translate (traduzPosicaoX x) (traduzPosicaoY y) $ Scale  1 2 $ fromJust $ lookup "Escada" images, desenhaLinhaMapa images t ]
                Vazio -> Pictures      [Translate (traduzPosicaoX x) (traduzPosicaoY y) $ Scale 1 1 $ fromJust $ lookup "Vazio" images, desenhaLinhaMapa images t ]
                Alcapao -> Pictures    [Translate (traduzPosicaoX x) (traduzPosicaoY y) $ Scale 1 1 $ fromJust $ lookup "Alcapao" images , desenhaLinhaMapa images t ]

desenhaEstado :: Imagens -> World -> Picture 
desenhaEstado images (World menu1 (jogo@(Jogo mapa@(Mapa (posi,dir) posf blocos) inimigos colecionaveis jogador)) menugameover menuvitoria menuatual)   
                        
                        |menuatual == "nomenu" =  desenhaMenu menu1 images
                        |menuatual == "jogo" =
                            pictures [ fromJust (lookup "Fundo" images),
                            desenhaMapa (jogo, images) (fazMatriz blocos 0.5),
                            desenhaJogador images jogador,
                            desenhaObjetivo images colecionaveis,
                            desenhaInimigos images inimigos,
                            desenhaVitoria images posf]
                        |menuatual == "vitoria" = desenhawin menuvitoria images
                        |menuatual == "gameover" = desenhalose menugameover images

carregarImagens :: IO Imagens
carregarImagens = do
        plataforma <- loadBMP "resources/blocolua.bmp"
        escadas <- loadBMP "resources/grey-ladder.bmp"
        alcapao <- loadBMP "resources/alcapao.bmp"
        peach <- loadBMP "resources/peach.bmp"
        fantasma <- loadBMP "resources/Fantasma.bmp"
        menuinicial <- loadBMP "resources/fundomenu.bmp"
        fundo <- loadBMP "resources/fundolua2.bmp"
        armadodireita <- loadBMP "resources/armadodireita.bmp"
        armadoesquerda <- loadBMP "resources/armadoesquerda.bmp"
        mariodireita <- loadBMP "resources/mariodireita.bmp"
        marioesquerda <- loadBMP "resources/marioesquerda.bmp"
        fundovitoria <- loadBMP "resources/fundovitoria.bmp"
        fundogameover <- loadBMP "resources/gameover.bmp"
        letrasmenu <- loadBMP "resources/letrasmenu.bmp"
        martelo <- loadBMP "resources/martelo.bmp"
        macaco  <- loadBMP "resources/macaco.bmp"
        moeda  <- loadBMP "resources/Moeda.bmp"
        let imagens = [ ("Plataforma", scale 0.5 0.5625 $ plataforma),
                        ("Escada", scale 1.3 1 $ escadas),
                        ("Alcapao", scale 1 1 $ alcapao),
                        ("Vazio", Blank),
                        ("Peach", scale 1 1 $ peach),
                        ("Fantasma", scale 0.2 0.2 $ fantasma),
                        ("Menu", scale 1 1 $ menuinicial),
                        ("Fundo", scale 1 1 $ fundo),
                        ("ArmadoDireita", scale 1 1 $ armadodireita),
                        ("ArmadoEsquerda", scale 1 1 $ armadoesquerda),
                        ("MarioDireita", scale 1 1 $ mariodireita),
                        ("MarioEsquerda", scale 1 1 $ marioesquerda),
                        ("FundoVitoria", scale 1 1 $ fundovitoria),
                        ("FundoGameOver", scale 1 1 $ fundogameover),
                        ("LetrasMenu", scale 1 1 $ letrasmenu),
                        ("Martelo", scale 1 1 $ martelo),
                        ("Macaco", scale 1 1 $ macaco),
                        ("Moeda", scale 1 1 $ moeda)
                        ]
        return imagens

main :: IO ()
main = do 
        images <- carregarImagens 
        play 
                FullScreen                     
                black               
                fr                      
                (estadoInicial images)
                (desenhaEstado images)        
                reageEvento               
                reageTempo
                
{-
colisaoPlataforma :: Jogo -> Bool
colisaoPlataforma j@(Jogo mapa@(Mapa (posi,dir) posf blocos) inimigos colecionaveis jogador)    | y<=(-33.1) && y>=(-33.9) && x>=(-51.7) && x<=(51.9) = True 
                                                                                                | (y>=(-4) && y<=(-3) && x>=(-51.7) && x<=(-7.7)) || (x>= (-2.4) && x<=(51.9) && y>=(-3.5) && y<=(-3)) = True
                                                                                                | otherwise = False
                where 
                    (x,y) = posicao jogador


colisaoEscada :: Jogo -> Bool
colisaoEscada j@(Jogo mapa@(Mapa (posi,dir) posf blocos) inimigos colecionaveis jogador) | y>=(-100) && y<=(-3.1) && x>=(33) && x<=(35) = True
                                                                                         | otherwise = False  
                                                                where 
                                                                    (x,y) = posicao jogador 

moveFantasmas :: [Personagem] -> [Personagem]
moveFantasmas [] = []
moveFantasmas ((Personagem v Fantasma (x, y) d tam e r w p (False, 0)):t) = (Personagem v Fantasma (newX, y) newD tam e r w p (False, 0)) : moveFantasmas t
                                        where 
                                            newX = if x >= 51.9 then -51.7 else if x <= -51.7 then 51.9 else x + (if d == Este then 0.5 else -0.5)
                                            newD = if newX >= 51.9 || newX <= -51.7 then if d == Este then Oeste else Este else d
moveFantasmas (h:t) = h : moveFantasmas t

reageEvento (EventKey (SpecialKey KeyLeft) Down _ _) j@(Jogo m inimigos colecionaveis (Personagem pos t (x, y) dir tam e r v p (c, d))) = if colisaoEscada j then j else (j { jogador = Personagem pos t (x - 3.5, y) Oeste tam e r v p (c, d) }) 
reageEvento (EventKey (SpecialKey KeyRight) Down _ _) j@(Jogo m inimigos colecionaveis (Personagem pos t (x, y) dir tam e r v p (c, d))) = if colisaoEscada j then j else  (j { jogador = Personagem pos t (x + 3.5, y) Este tam e r v p (c, d) })
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) j@(Jogo m inimigos colecionaveis (Personagem pos t (x, y) dir tam e r v p (c, d))) = if colisaoPlataforma j || colisaoEscada j then (j { jogador = Personagem pos t (x , y + 10) Este tam e r v p (c, d) }) else j
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) j@(Jogo m inimigos colecionaveis (Personagem pos t (x, y) dir tam e r v p (c, d))) = if colisaoPlataforma j || (colisaoEscada j == False)  then j else (j { jogador = Personagem pos t (x , y - 5) Este tam e r v p (c, d) }) 
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) j@(Jogo m inimigos colecionaveis (Personagem pos t (x, y) dir tam e r v p (c, d))) = if colisaoPlataforma j || (colisaoEscada j == False)  then j else (j { jogador = Personagem pos t (x , y - 5) Este tam e r v p (c, d) })


aplicaGravidade :: Float -> Jogo -> Jogo
aplicaGravidade _ jogo@(Jogo mapa inimigos colecionaveis jogador) = if colisaoPlataforma jogo then jogo else jogo { jogador = jogador { posicao = (xs, ys+vy+(-0.4))} }
    where
        (xs, ys) = posicao jogador  
        (vx, vy) = velocidade jogador 
-}