module UI (drawState) where

import Graphics.Gloss
import Types

cardDrawing = Translate (0) (-100) -- shift the card into the middle
  $ Scale 0.5 0.5 -- display it half the original size
  $ rectangleWire 400 600

drawWeekText :: Int -> Picture
drawWeekText num = Translate (-100) (300) -- shift the start of the text to the left of the window
  $ Scale 0.25 0.25
  $ Text ("Week: "++show num)

drawCardText :: String -> Picture
drawCardText s = Translate (-350) (100) -- shift the start of the text to the left of the window
  $ Scale 0.25 0.25
  $ Text s

drawCard :: CurrentKey -> Picture
drawCard None = cardDrawing
drawCard Types.Left = Translate (-80) (0)
  $ Rotate (-12)
  $ cardDrawing
drawCard Types.Right = Translate (80) (0)
  $ Rotate (12)
  $ cardDrawing

-- todo: complete so it actually updates based on the resource values, turns the card, shows the resource symbols
-- note: don't worry too much about the text looking perfect, i think it would be too hard to really dig into
drawState :: State -> Picture 
drawState (currentKey, (text, _, _), _, _, week) = Pictures [drawCardText text, drawCard currentKey, drawWeekText week]