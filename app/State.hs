module State (inputHandler, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game -- for Key-handling
import Types

initialCard :: Card
initialCard = ("left or right, press again to confirm", (0,0,0,0), (0,0,0,0))

initialState :: State
initialState = (Types.None, initialCard, (10,20,5,8), 0)

deck :: [Card]
deck = [
  ("A", (0,-1,0,1), (0,1,0,-1)),
  ("B", (3,-4,0,1), (0,6,0,0))]

inputHandler :: Event -> State -> State
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) state = handleLeftKey state
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) state = handleRightKey state
inputHandler _ s = s

-- todo: actually make changes beyond updating CurrentKey
handleLeftKey :: State -> State
handleLeftKey (Types.Left, card, resources, week) = (Types.None, card, resources, week + 1)
handleLeftKey (Types.Right, card, resources, week) = (Types.None, card, resources, week)
handleLeftKey (Types.None, card, resources, week) = (Types.Left, card, resources, week)

-- todo: actually make changes beyond updating CurrentKey
handleRightKey :: State -> State
handleRightKey (Types.Right, card, resources, week) = (Types.None, card, resources, week + 1)
handleRightKey (Types.Left, card, resources, week) = (Types.None, card, resources, week)
handleRightKey (Types.None, card, resources, week) = (Types.Right, card, resources, week)