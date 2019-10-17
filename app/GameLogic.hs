module GameLogic (deck, processMove, initialCard) where
  import Types

  deck :: [Card]
  deck = [
    ("A", (0,-1,0,1), (0,1,0,-1)),
    ("B", (3,-4,0,1), (0,6,0,0)),
    ("C", (-7,-1,4,1), (5,-5,0,0)),
    ("B", (3,-4,0,1), (0,6,0,0)),
    ("A", (0,-1,0,1), (0,1,0,-1)),
    ("B", (3,-4,0,1), (0,6,0,0)),
    ("A", (0,-1,0,1), (0,1,0,-1)),
    ("B", (3,-4,0,1), (0,6,0,0))]

  deckIncrement = 3 

  initialCard :: Card
  initialCard = ("left or right, press again to confirm", (0,0,0,0), (0,0,0,0))

  -- Card given to the user at the end of the game so they can quit or restart
  endGameCard :: Card
  endGameCard = ("Game Over. Press right to play again.", (0,0,0,0), (0,0,0,0))
  
  processMove :: State -> State
  processMove state =
      let (key, card, cardIndex, resources, week) = state in
        let (text, leftAction, rightAction) = card in
          if key == Types.Left
            then
              if (text == "Game Over. Press right to play again.")
                then (Types.None, endGameCard, cardIndex, resources, week) -- TODO: implement quitting maybe?
              else if (lostGame (updateResources leftAction resources) || (wonGame week))
                then (Types.None, endGameCard, cardIndex, resources, week) -- Game over
                else (Types.None, (getNextCard cardIndex), cardIndex + deckIncrement, (updateResources leftAction resources), week + 1)
          else if key == Types.Right
            then
              if (text == "Game Over. Press right to play again.")
                then (Types.None, initialCard, cardIndex + deckIncrement, (20,20,20,20), 0) -- Initial state but with next card instead
              else if (lostGame (updateResources rightAction resources) || (wonGame week))
                then (Types.None, endGameCard, cardIndex, resources, week) -- Game over
                else (Types.None, (getNextCard cardIndex), cardIndex + deckIncrement, (updateResources rightAction resources), week + 1)
          else 
            if (wonGame week) 
              then (Types.None, endGameCard, cardIndex, resources, week) -- Game over
              else state -- No move made

  getNextCard :: Int -> Card
  getNextCard index = deck !! ((index + deckIncrement) `mod` length(deck))

  -- Adds the amount of resources in the action to the resources passed in
  updateResources :: Action -> Resources -> Resources
  updateResources action resources = let (changeSleep, changeGrades, changeMoney, changeSocial) = action
                                        in let (sleep, grades, money, social) = resources
                                        in (incrementOrCapResource sleep changeSleep, incrementOrCapResource grades changeGrades, incrementOrCapResource money changeMoney, incrementOrCapResource social changeSocial)
  
  -- Adds the change in resources ot the current resource amount.
  -- If the value after adding the change is greater than 20, truncates the value to 20.
  incrementOrCapResource :: Int -> Int -> Int
  incrementOrCapResource cur delta
    | (cur + delta) > 20 = 20
    | otherwise = (cur + delta)

  -- Function that, given the game's state, checks if any of the resources are 0
  lostGame :: Resources -> Bool
  lostGame resources = let (sleep, grades, money, social) = resources
                      in (sleep <= 0 || grades <= 0 || money <= 0 || social <= 0)

  -- Function that returns true if the player survived 30 weeks
  wonGame :: Week -> Bool
  wonGame week = (week >= 30)