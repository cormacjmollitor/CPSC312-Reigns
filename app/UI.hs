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

getSleep :: (a, b, c, d) -> a
getSleep (x, _, _, _) = x

getGrades :: (a, b, c, d) -> b 
getGrades (_, x, _, _) = x

getMoney :: (a, b, c, d) -> c
getMoney (_, _, x, _) = x

getSocialLife :: (a, b, c, d) -> d 
getSocialLife (_, _, _, x) = x

barIncrementX = 100

sleepBarX = -200
sleepBarY = 200
sleepTextX = sleepBarX - 25
sleepTextY = sleepBarY - 50
actionIconY = sleepBarY + 50

drawResources :: (Int, Int, Int, Int) -> Picture
drawResources (sleep, grades, money, socialLife) = Pictures [
  translate (sleepBarX) (sleepBarY) $ drawResourceBar sleep,
  translate (sleepTextX) (sleepTextY) $ scale 0.1 0.1 $ text ("Sleep"),
  translate (sleepBarX + (barIncrementX * 1)) (sleepBarY) $ drawResourceBar grades,
  translate (sleepTextX + (barIncrementX * 1)) (sleepTextY) $ scale 0.1 0.1 $ text ("Grades"),
  translate (sleepBarX + (barIncrementX * 2)) (sleepBarY) $ drawResourceBar money,
  translate (sleepTextX + (barIncrementX * 2)) (sleepTextY) $ scale 0.1 0.1 $ text ("Money"),
  translate (sleepBarX + (barIncrementX * 3)) (sleepBarY) $ drawResourceBar socialLife,
  translate (sleepTextX + (barIncrementX * 3)) (sleepTextY) $ scale 0.1 0.1 $ text ("Social life")]

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

drawActionIcons :: (Int, Int, Int, Int) -> Picture
drawActionIcons (sleep, grades, money, socialLife) = pictures [
  translate (sleepBarX) (actionIconY) $ scale 0.1 0.1 $ drawSingleIcon sleep,
  translate (sleepBarX + (barIncrementX * 1)) (actionIconY) $ scale 0.1 0.1 $ drawSingleIcon grades,
  translate (sleepBarX + (barIncrementX * 2)) (actionIconY) $ scale 0.1 0.1 $ drawSingleIcon money,
  translate (sleepBarX + (barIncrementX * 3)) (actionIconY) $ scale 0.1 0.1 $ drawSingleIcon socialLife]

drawSingleIcon :: Int -> Picture
drawSingleIcon change = Text $ show change

-- todo: complete so it actually updates based on the resource values, turns the card, shows the resource symbols
-- note: don't worry too much about the text looking perfect, i think it would be too hard to really dig into
drawState :: State -> Picture 
drawState (currentKey, (text, leftAction, rightAction), resources, week) = Pictures [drawCardText text, drawCard currentKey, drawResources resources, drawActionIcons leftAction, drawActionIcons rightAction, drawWeekText week]
