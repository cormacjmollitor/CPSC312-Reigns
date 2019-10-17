module GameLogic (deck, processMove, initialCard) where
  import Types
  import System.Random

  deck :: [Card]
  deck = [
    ("A", (0,-1,0,1), (0,1,0,-1)),
    ("B", (3,-4,0,1), (0,6,0,0)),
    ("C", (-7,-1,4,1), (5,-5,0,0)),
    ("D", (3,-4,0,1), (0,6,0,0)),
    ("E", (0,-1,0,1), (0,1,0,-1)),
    ("F", (3,-4,0,1), (0,6,0,0)),
    ("G", (0,-1,0,1), (0,1,0,-1)),
    ("H", (3,-4,0,1), (0,6,0,0))]

  initialCard :: Card
  initialCard = ("left or right, press again to confirm", (0,0,0,0), (0,0,0,0))

  -- Card given to the user at the end of the game so they can quit or restart
  endGameCard :: Card
  endGameCard = ("Game Over. Press right to play again.", (0,0,0,0), (0,0,0,0))
  
  processMove :: State -> IO State
  processMove state =
    do
      let (key, card, resources, week) = state
      let (text, leftAction, rightAction) = card
      nextCard <- getNextCard
      if key == Types.Left
        then
          if (text == "Game Over. Press right to play again.")
            then return (Types.None, endGameCard, resources, week) -- TODO: implement quitting maybe?
          else if (lostGame (updateResources leftAction resources) || (wonGame week))
            then  return (Types.None, endGameCard, resources, week) -- Game over
            else  return (Types.None, nextCard, (updateResources leftAction resources), week + 1)
      else if key == Types.Right
        then
          if (text == "Game Over. Press right to play again.")
            then return (Types.None, initialCard, (20,20,20,20), 0) -- Initial state but with next card instead
          else if (lostGame (updateResources rightAction resources) || (wonGame week))
            then return (Types.None, endGameCard, resources, week) -- Game over
            else return (Types.None, nextCard, (updateResources rightAction resources), week + 1)
      else 
        if (wonGame week) 
          then return (Types.None, endGameCard, resources, week) -- Game over
          else return state -- No move made

  -- Gets a random card from the deck
  getNextCard :: IO Card
  getNextCard =
    do
      index <- randomRIO(0, length(deck) - 1) :: IO Int
      return $ deck !! index
    

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