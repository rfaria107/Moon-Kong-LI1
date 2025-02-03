module Main where

import Test.HUnit
import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Mapa

main :: IO ()
main = runTestTTAndExit $ test [testesTarefa1, testesTarefa2]

-- | Colisoes entre personagens

p1 = Personagem (0,0) Jogador (4,4) Este (0.5,0.5) False False 10 0 (False, 0.0)
p2 = Personagem (0,0) Jogador (3.8,4) Oeste (0.5,0.5) False False 3 0 (False,0)
teste1 = "T1: Personagens colidem " ~: True ~=? colisoesPersonagens p1 p2

p3 = Personagem (0,0) Jogador (2,7) Este (0.5,0.5) False False 10 0 (False, 0.0)
p4 = Personagem (0,0) Fantasma (2,2) Oeste (0.5,0.5) True False 2 0 (False, 0.0)

teste2 = "T2: Personagens nao colidem " ~: False ~=? colisoesPersonagens p3 p4

-- | Colisoes com paredes, o mapa utilizado é o mapa presente no ficheiro Mapa.hs

pl1 = Personagem (0.0,0.0) Jogador (8.5,6.5) Este (0.5,0.5) False False 10 0 (False, 0.0)

teste3 = "T3: Jogador nao colide com nenhuma parede " ~: False ~=? colisoesParede mapa1 pl1

pl2 = Personagem (0.0,0.0) Jogador (21,6.5) Este (0.5,0.5) False False 10 0 (False, 0.0)

teste4 = "T4: Jogador colide com limite lateral " ~: True ~=? colisoesParede mapa1 pl2

pl3 = Personagem (0.0,0.0) Jogador (5,-0.1) Este (0.5,0.5) False False 10 0 (False, 0.0)

teste5 = "T5: Jogador colide com limite superior " ~: True ~=? colisoesParede mapa1 pl3

pl4 = Personagem (0.0,0.0) Jogador (7.5,2) Este (0.5,0.5) False False 10 0 (False, 0.0)

teste6 = "T6: Jogador colide com plataforma " ~: True ~=? colisoesParede mapa1 pl4

-- | Testes da tarefa 1
testesTarefa1 = test [teste1, teste2, teste3, teste4, teste5, teste6 ]

-- | Testes da tarefa 2

definejogo1 :: Jogo
definejogo1 = Jogo m i c j
                where   m = mapa1
                        i = [inimigo1,inimigo2,inimigo3]
                        c = [(Moeda,(15,5)),(Martelo,(17,8.5))]
                        j = jogador1
                        
teste7 = "T7: Jogo que respeita todas as regras é válido" ~: True ~=? valida definejogo1 -- ^ utiliza o jogo definido na main

testesTarefa2 = test [teste7]
