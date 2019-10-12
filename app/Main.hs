module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game -- for Key-handling

type Week = Int
type Action = (Int, Int, Int, Int)
type Card = (String, Action, Action)
type Resources = (Int, Int, Int, Int)
data CurrentKey = Left | Right | None
type State = (CurrentKey, Card, Resources, Week)
-- type State = (Float, Float)

initialCard :: Card
initialCard = ("left or right, press again to confirm", (0,0,0,0), (0,0,0,0))

initialState :: State
initialState = (None, initialCard, (20,20,20,20), 0)

deck :: [Card]
deck = [
  ("A", (0,-1,0,1), (0,1,0,-1)),
  ("B", (3,-4,0,1), (0,6,0,0))]

simulationStepsPerSecond :: Int
simulationStepsPerSecond = 1

windowDisplay :: Display
windowDisplay = FullScreen

main :: IO ()
main = play
  windowDisplay
  white
  simulationStepsPerSecond
  initialState
  drawingFunc
  inputHandler
  updateFunc

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
drawingFunc :: State -> Picture 
drawingFunc (currentKey, (text, _, _), _, _) = Pictures [getText text, getCard currentKey]

-- todo: actually setup the key actions
-- Currently only changes cards upon left-arrow
inputHandler :: Event -> State -> State
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) state = state
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) state = state
inputHandler _ s = s

-- Do nothing, we only respond to key events
updateFunc :: Float -> State -> State
updateFunc _ state = state
