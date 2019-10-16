module State (inputHandler, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game -- for Key-handling
import Types
import System.Random
import System.IO.Unsafe
import GameLogic

seedIndex = (unsafePerformIO (randomRIO(0, length(deck) - 1) :: IO Int))

initialState :: State
initialState = (Types.None, initialCard, seedIndex, (20,20,20,20), 0)

inputHandler :: Event -> State -> State
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) state = handleLeftKey state
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) state = handleRightKey state
inputHandler _ s = s

-- todo: actually make changes beyond updating CurrentKey
handleLeftKey :: State -> State
handleLeftKey (Types.Left, card, cardIndex, resources, week) = processMove (Types.Left, card, cardIndex, resources, week) -- This will call the backend
handleLeftKey (Types.Right, card, cardIndex, resources, week) = (Types.None, card, cardIndex, resources, week) -- following 2 will not call backend
handleLeftKey (Types.None, card, cardIndex, resources, week) = (Types.Left, card, cardIndex, resources, week)

-- todo: actually make changes beyond updating CurrentKey
handleRightKey :: State -> State
handleRightKey (Types.Right, card, cardIndex, resources, week) = processMove (Types.Right, card, cardIndex, resources, week) -- This will call the backend
handleRightKey (Types.Left, card, cardIndex, resources, week) = (Types.None, card, cardIndex, resources, week)
handleRightKey (Types.None, card, cardIndex, resources, week) = (Types.Right, card, cardIndex, resources, week)