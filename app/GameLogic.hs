module GameLogic where
  import System.Random
  import Types

  -- This is currently in State.hs so we should choose whether to put it here or there
  deck :: [Card]
  deck = [
    ("A", (0,-1,0,1), (0,1,0,-1)),
    ("B", (3,-4,0,1), (0,6,0,0))]
  
  -- Card given to the user at the end of the game so they can quit or restart
  endGameCard :: Card
  endGameCard = ("Game Over", (0,0,0,0), (0,0,0,0))
  
  processMove :: CurrentKey -> Action -> State -> IO Result
  processMove key action state =
    do
      nextCard <- getRandomCard
      let (currentKey, card, resources, week) = state
      if key == Types.Left
        then
          if (lostGame (decrementResources action resources) || (wonGame week)) 
            then return (EndOfGame week)
            else return (ContinueGame (Types.None, nextCard, (decrementResources action resources), week + 1))
        else if key == Types.Right
          then
            if (lostGame (incrementResources action resources) || (wonGame week))
              then return (EndOfGame week)
              else return (ContinueGame (Types.None, nextCard, (incrementResources action resources), week + 1))
        else 
          if (wonGame week) 
            then return (EndOfGame week) 
            else return (ContinueGame state) -- No move made
  
  getRandomCard :: IO Card
  getRandomCard = 
    do
      index <- randomRIO(0, length(deck) - 1) :: IO Int
      return $ deck !! index

  -- It'd be best practice to add a function variable and consolidate these two functions
  -- but fuck it we'll do it live.
  incrementResources :: Action -> Resources -> Resources
  incrementResources action resources = let (changeSleep, changeGrades, changeMoney, changeSocial) = action
                                        in let (sleep, grades, money, social) = resources
                                        in (sleep + changeSleep, grades + changeGrades, money + changeMoney, social + changeSocial)

  decrementResources :: Action -> Resources -> Resources
  decrementResources action resources = let (changeSleep, changeGrades, changeMoney, changeSocial) = action
                                        in let (sleep, grades, money, social) = resources
                                        in (sleep - changeSleep, grades - changeGrades, money - changeMoney, social - changeSocial)

  -- Function that, given the game's state, checks if any of the resources are 0
  lostGame :: Resources -> Bool
  lostGame resources = let (sleep, grades, money, social) = resources
                      in (sleep <= 0 || grades <= 0 || money <= 0 || social <= 0)
  
  -- Function that returns true if the player survived 30 weeks
  wonGame :: Week -> Bool
  wonGame week = (week == 30)