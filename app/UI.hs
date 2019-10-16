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

drawResources :: Resources -> Picture
drawResources resources = Pictures [
  showResources resources
  -- Translate (-400) (200)
  -- $ Scale 0.25 0.25
  -- $ Text (showResources resources)
  -- , translate (-400) (200) $ drawResourceBar (getSleep resources)
  ]

getSleep :: (a, b, c, d) -> a
getSleep (x, _, _, _) = x

getGrades :: (a, b, c, d) -> b 
getGrades (_, x, _, _) = x

getMoney :: (a, b, c, d) -> c
getMoney (_, _, x, _) = x

getSocialLife :: (a, b, c, d) -> d 
getSocialLife (_, _, _, x) = x

-- showResources :: (Int, Int, Int, Int) -> String
-- showResources (sleep, grades, money, socialLife) =
--   "Sleep: " ++ show sleep ++ " | Grades: " ++ show grades ++ " | Money: " ++ show money ++ " | Social life: " ++ show socialLife

showResources :: (Int, Int, Int, Int) -> Picture
showResources (sleep, grades, money, socialLife) = Pictures [
  translate (-400) (200) $ drawResourceBar sleep,
  translate (-350) (200) $ drawResourceBar grades,
  translate (-300) (200) $ drawResourceBar money,
  translate (-250) (200) $ drawResourceBar socialLife]

resourceBarOutline = rectangleWire 10 40

drawResourceBarFill :: Int -> Picture
drawResourceBarFill points = rectangleSolid 10 (fromIntegral points * 2)

drawResourceBar :: Int -> Picture
drawResourceBar points = pictures [drawResourceBarFill points, resourceBarOutline]

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
drawState (currentKey, (text, _, _), resources, week) = Pictures [drawCardText text, drawCard currentKey, drawResources resources, drawWeekText week]