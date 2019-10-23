module GameLogic (deck, processMove, initialCard) where
  import Types
  import System.Random

-- sleep / grades / money / social
-- no / yes
  deck :: [Card]
  deck = [
    ("Take a nap", (-5,-3,0,0), (7,-3,0,0)),
    ("Go to bed on time", (-8,-4,0,0), (10,0,0,0)),
    ("Sleep in on Sunday", (-5,-5,0,0), (8,0,0,0)),
    ("Take sleeping pills", (-4,4,0,0), (7,-5,0,0)),
    ("Sleep through lecture", (-4,1,0,0), (5,-3,0,0)),
    ("Study all night", (8,-3,0,0), (-18,7,0,-5)),
    ("Study at the library", (0,-5,0,5), (0,10,0,-5)),
    ("Pick up a shift", (3,3,-5,0), (-8,-3,10,0)),
    ("Create a website on contract", (0,0,0,0), (-8,-8,20,0)),
    ("Pick up a night shift", (3,3,-5,0), (-8,-10,15,0)),
    ("Sell a kidney", (0,0,-5,0), (-3,-6,20,0)),
    ("Call in sick to work", (0,-3,10,0), (0,6,-5,0)),
    ("Go out to the bar", (4,1,0,-12), (-8,-2,-8,12)),
    ("Go to a concert", (2,0,0,-5), (-2,-1,-12,8)),
    ("Go on a date", (4,0,0,-7), (-5,0,-7,14)),
    ("Buy a Juul", (0,0,0,0), (0,-3,-10,6)),
    ("Skip tutorial to get lunch with friends", (0,3,0,-5), (0,-6,-4,3)),
    ("Go to the movies", (3,0,4,-12), (-5,-6,-6,8)),
    ("Go for dinner with your parents", (0,2,0,-18), (0,-12,0,6)),
    ("Workout", (-8,3,0,-4), (0,-6,-3,4)),
    ("Buy a Patagonia rain jacket", (0, 0, 0, -3), (0, 0, -10, 3)),
    ("Move to a nicer apartment", (-6, -3, 6, 0), (5, 5, -10, 4)),
    ("Subscribe to Headspace", (-3, -3, 5, 0), (7, 3, -5, 2)),
    ("Go to Seattle for reading week", (8, 5, 3, -7), (-4, -3, -5, 10)),
    ("Become an exec of an AMS club", (4, 4, 0, -6), (-3, -4, 0, 7)),
    ("Go to Pit Night", (4,3,0,-5), (-5,-8,-3,8)),
    ("Go to My Home Cuisine with friends", (0,2,0,-3), (-3,-3,-3,6)),
    ("Get a Chegg account", (0,-4,0,0), (3,9,-5,3))]

  endGameText :: String
  endGameText = "Game Over. Press right to play again."

  initialCard :: Card
  initialCard = ("left or right, press again to confirm", (0,0,0,0), (0,0,0,0))

  -- Card given to the user at the end of the game so they can quit or restart
  endGameCard :: Card
  endGameCard = (endGameText, (0,0,0,0), (0,0,0,0))
  
  processMove :: State -> IO State
  processMove state =
    do
      let (key, card, resources, week) = state
      let (text, leftAction, rightAction) = card
      nextCard <- getNextCard
      if key == Types.Left
        then
          if (text == endGameText)
            then return (Types.None, endGameCard, resources, week) -- TODO: implement quitting maybe?
          else if (lostGame (updateResources leftAction resources) || (wonGame week))
            then  return (Types.None, endGameCard, (updateResources leftAction resources), week) -- Game over
            else  return (Types.None, nextCard, (updateResources leftAction resources), week + 1)
      else if key == Types.Right
        then
          if (text == endGameText)
            then return (Types.None, initialCard, (15,15,15,15), 0) -- Initial state but with next card instead
          else if (lostGame (updateResources rightAction resources) || (wonGame week))
            then return (Types.None, endGameCard, (updateResources rightAction resources), week) -- Game over
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