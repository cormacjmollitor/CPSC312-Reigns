module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import UI
import State
import Types

simulationStepsPerSecond :: Int
simulationStepsPerSecond = 1

windowDisplay :: Display
windowDisplay = FullScreen

main :: IO ()
main = playIO
  windowDisplay
  white
  simulationStepsPerSecond
  initialState
  drawState -- from UI.hs
  inputHandler -- from State.hs
  updateFunc

-- Do nothing, we only respond to key events
updateFunc :: Float -> State -> IO State
updateFunc _ state = do return state
