module State (inputHandler, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game -- for Key-handling
import Types

initialCard :: Card
initialCard = ("left or right, press again to confirm", (0,0,0,0), (0,0,0,0))

initialState :: State
initialState = (None, initialCard, (20,20,20,20), 0)

deck :: [Card]
deck = [
  ("A", (0,-1,0,1), (0,1,0,-1)),
  ("B", (3,-4,0,1), (0,6,0,0))]

-- todo: actually setup the key actions
-- Currently only changes cards upon left-arrow
inputHandler :: Event -> State -> State
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) state = state
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) state = state
inputHandler _ s = s