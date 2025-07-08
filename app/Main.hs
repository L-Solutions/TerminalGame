{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Terminal.Game

type Position = (Int, Int)
type GameState = ([Char], Position)

version :: String
version = "v0.2"

width :: Width
width = 8

height :: Height
height = 8

coord :: Coords
coord = (2*width + 1, height)

lightCell :: Plane
lightCell = invert $ color Yellow Dull $ cell ' '

board :: Plane
board = makeTransparent ' ' $ mergePlanes canvas lightCells
    where canvas = blankPlane (fst coord) (snd coord)
          lightCells = [ ((y,x), lightCell) | y <- [1 .. snd coord], m <- [(y - 1) `mod` 3], x <- [ 1 + m, 3 + m .. fst coord + m]]

renderInfo :: Position -> [String]
renderInfo (x,y) = let row = fst coord
                    in [ replicate row '-'
                       , " x = " ++ show x
                       , " y = " ++ show y
                       , replicate row ' '
                       , replicate row ' '
                       , replicate row ' '
                       , replicate row ' '
                       , replicate row '-'
                       ]

drawInfo :: Position -> Plane
drawInfo pos = stringPlane $ unlines $ renderInfo pos

-- Génère le contenu texte de l'écran avec le curseur @ à la position donnée
renderText :: Position -> [String]
renderText (x, y) = let motif n = concat $ replicate n " |" in
  [ if row == y
      then "|" ++ motif x ++ "@|" ++ motif (width - x - 1)
      else "|" ++ motif width
  | row <- [0 .. height - 1]
  ]

-- Génère une Plane à partir du texte rendu
drawCursor :: Position -> Plane
drawCursor pos = stringPlaneTrans ' ' $ unlines $ renderText pos

-- Gère les séquences d’échappement ANSI pour les flèches
handleArrowSeq :: [Char] -> Position -> Position
handleArrowSeq ['\ESC','[','A'] (x, y) = (x, max 0 (y - 1))               -- ↑
handleArrowSeq ['\ESC','[','B'] (x, y) = (x, min (height - 1) (y + 1))    -- ↓
handleArrowSeq ['\ESC','[','C'] (x, y) = (min (width - 1) (x + 1), y)     -- →
handleArrowSeq ['\ESC','[','D'] (x, y) = (max 0 (x - 1), y)               -- ←
handleArrowSeq _ pos = pos

-- Logique de mise à jour : prend aussi GEnv désormais
gameLogic :: GEnv -> GameState -> Event -> Either () GameState
gameLogic _ (buf, pos) ev =
  case ev of
    Tick -> Right ([], pos)
    KeyPress c ->
      let buf'  = buf ++ [c]
          pos'  = handleArrowSeq buf' pos
      in if c == 'q'
         then Left ()
         else Right (if length buf' >= 3 then [] else buf', pos')

-- Fonction de dessin : prend aussi GEnv
draw :: GEnv -> GameState -> Plane
draw _ (_, pos) = let label = (invert $ color Green Dull $ stringPlane version)
                   --in (drawCursor pos ||| (drawInfo pos & (5,2) % label))
                   in (board & pos % cell '@') ||| (drawInfo pos & (5,2) % label)
-- Lancement du jeu
main :: IO ()
main = playGame Game
  { gTPS           = 20
  , gInitState     = ([], (0, 0))
  , gLogicFunction = gameLogic
  , gDrawFunction  = draw
  }
