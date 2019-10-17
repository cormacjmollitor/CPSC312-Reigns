module State (inputHandler, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game -- for Key-handling
import Types
import GameLogic

initialResources :: Resources
initialResources = (15,15,15,15)

initialState :: State
initialState = (Types.None, initialCard, initialResources, 0) -- base game
-- initialState = (Types.None, initialCard, (1,1,1,1), 12) -- almost lost
-- initialState = (Types.None, initialCard, (20,20,20,20), 29) -- almost lost

inputHandler :: Event -> State -> IO State
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) state = handleLeftKey state
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) state = handleRightKey state
inputHandler _ s = do return s

handleLeftKey :: State -> IO State
handleLeftKey (Types.Left, card, resources, week) = processMove (Types.Left, card, resources, week) -- No action
handleLeftKey (Types.Right, card, resources, week) = do return (Types.None, card, resources, week) -- update CurrentKey
handleLeftKey (Types.None, card, resources, week) = do return (Types.Left, card, resources, week) -- update CurrentKey

handleRightKey :: State -> IO State
handleRightKey (Types.Right, card, resources, week) = processMove (Types.Right, card, resources, week) -- Yes action
handleRightKey (Types.Left, card, resources, week) = do return (Types.None, card, resources, week) -- update CurrentKey
handleRightKey (Types.None, card, resources, week) = do return (Types.Right, card, resources, week) -- update CurrentKey