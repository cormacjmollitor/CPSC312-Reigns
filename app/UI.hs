module UI (drawState) where

import Graphics.Gloss
import Types

getText :: String -> Picture
getText s = Translate (-350) (200) -- shift the text to the middle of the window
  $ Scale 0.25 0.25 -- display it half the original size
  $ Text s

getCard :: CurrentKey -> Picture
getCard card = Translate (1) (-100) -- shift the card
  $ Scale 0.5 0.5 -- display it half the original size
  $ rectangleWire 400 600

-- todo: complete so it actually updates based on the resource values, turns the card, shows the resource symbols
-- note: don't worry too much about the text looking perfect, i think it would be too hard to really dig into
drawState :: State -> Picture 
drawState (currentKey, (text, _, _), _, _) = Pictures [getText text, getCard currentKey]