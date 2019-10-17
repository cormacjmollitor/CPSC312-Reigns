module State (inputHandler, initialState) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game -- for Key-handling
import Types
import GameLogic

initialResources :: Resources
initialResources = (15,15,15,15)

initialState :: State
initialState = (Types.None, initialCard, initialResources, 0)

inputHandler :: Event -> State -> IO State
inputHandler (EventKey (SpecialKey KeyLeft) Down _ _) state = handleLeftKey state
inputHandler (EventKey (SpecialKey KeyRight) Down _ _) state = handleRightKey state
inputHandler _ s = do return s

handleLeftKey :: State -> IO State
handleLeftKey (Types.Left, card, resources, week) = processMove (Types.Left, card, resources, week) -- This will call the backend
handleLeftKey (Types.Right, card, resources, week) = do return (Types.None, card, resources, week) -- following 2 will not call backend
handleLeftKey (Types.None, card, resources, week) = do return (Types.Left, card, resources, week)

handleRightKey :: State -> IO State
handleRightKey (Types.Right, card, resources, week) = processMove (Types.Right, card, resources, week) -- This will call the backend
handleRightKey (Types.Left, card, resources, week) = do return (Types.None, card, resources, week)
handleRightKey (Types.None, card, resources, week) = do return (Types.Right, card, resources, week)