{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Terminal.Game

type GameState = ([Char], Coords)

lastUpdate :: String
lastUpdate = "2025-08-09T16:51-0400"

version :: String
version = "v0.3.2"

width :: Width
width = 8

height :: Height
height = 8

sizeX :: Width
sizeX = 4

sizeY :: Height
sizeY = 2

coord :: Coords
coord = (2*width + 1, height)

lightCell :: Plane
lightCell = invert $ color White Dull $ cell ' '
darkCell :: Plane
darkCell = invert $ color Black Dull $ cell ' '

mark :: Plane
mark = makeTransparent ' ' $ l2 === l3
    where m1 = cell '#'
          m2 = cell '#'
          m0 = cell ' '
          l1 = m1 ||| m0 ||| m0 ||| m2
          l2 = m0 ||| m1 ||| m2 ||| m0
          l3 = m0 ||| m2 ||| m1 ||| m0
          l4 = m2 ||| m0 ||| m0 ||| m1

block :: Plane -> Plane
block c = foldl (===) (head blockY) (tail blockY)
    where blockY = take sizeY $ repeat $ foldl (|||) (head blockX) (tail blockX)
          blockX = take sizeX $ repeat c

board :: Width -> Height -> Plane
board width height = foldl (===) adjustRow $ replicate ((height - 1) `div` 2) (line 1 === line 0)
    where line p = foldl (|||) (adjustLine p) $ mconcat $ replicate (width `div` 2) $ pattern p
          adjustLine p = if even width then w else foldl (|||) w (drop 2 $ pattern p)
          adjustRow = if odd height then line 0 else line 1 === line 0
          pattern 0 = [block darkCell, w, block lightCell, w]
          pattern 1 = [block lightCell, w, block darkCell, w]
          w = let blockW = take sizeY $ repeat $ cell '|'
               in foldl (===) (head blockW) (tail blockW)

renderInfo :: Coords -> [String]
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

drawInfo :: Coords -> Plane
drawInfo coord = stringPlane $ unlines $ renderInfo coord

-- Génère le contenu texte de l'écran avec le curseur @ à la position donnée
renderText :: Coords -> [String]
renderText (x, y) = let motif n = concat $ replicate n " |" in
  [ if row == y
      then "|" ++ motif x ++ "@|" ++ motif (width - x - 1)
      else "|" ++ motif width
  | row <- [0 .. height - 1]
  ]

-- Génère une Plane à partir du texte rendu
drawCursor :: Coords -> Plane
drawCursor coord = stringPlaneTrans ' ' $ unlines $ renderText coord

-- Gère les séquences d’échappement ANSI pour les flèches
handleArrowSeq :: [Char] -> Coords -> Coords
handleArrowSeq ['\ESC','[','A'] (x, y) = (x, max 0 (y - 1))               -- ↑
handleArrowSeq ['\ESC','[','B'] (x, y) = (x, min (height - 1) (y + 1))    -- ↓
handleArrowSeq ['\ESC','[','C'] (x, y) = (min (width - 1) (x + 1), y)     -- →
handleArrowSeq ['\ESC','[','D'] (x, y) = (max 0 (x - 1), y)               -- ←
handleArrowSeq ['i'] (x, y) = (x, max 0 (y - 1))                          -- ↑
handleArrowSeq ['k'] (x, y) = (x, min (height - 1) (y + 1))               -- ↓
handleArrowSeq ['l'] (x, y) = (min (width - 1) (x + 1), y)                -- →
handleArrowSeq ['j'] (x, y) = (max 0 (x - 1), y)                          -- ←
handleArrowSeq _ coord = coord

-- Logique de mise à jour : prend aussi GEnv désormais
gameLogic :: GEnv -> GameState -> Event -> Either () GameState
gameLogic _ (buf, coord) ev =
  case ev of
    Tick -> Right ([], coord)
    KeyPress c ->
      let buf' = buf ++ [c]
          coord' = handleArrowSeq buf' coord
      in if c == 'q'
         then Left ()
         else Right (if length buf' >= 3 then [] else buf', coord')

-- Fonction de dessin : prend aussi GEnv
draw :: GEnv -> GameState -> Plane
draw _ (_, coord) = let label = (invert $ color Green Dull $ stringPlane version)
                     in (drawCursor coord ||| (drawInfo coord & (5,2) % label))
                        ===
                        (board width height & (1, 2) % mark & (1 + sizeY, 2 + (sizeX + 1)) % mark)

--
-- Lancement du jeu
main :: IO ()
main = playGame Game
  { gTPS           = 20
  , gInitState     = ([], (0, 0))
  , gLogicFunction = gameLogic
  , gDrawFunction  = draw
  }
