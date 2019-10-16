module Types (Week, Action, Card, Resources, CurrentKey (Left, Right, None), State, Result(EndOfGame, ContinueGame)) where

type Week = Int
type Action = (Int, Int, Int, Int) -- (Sleep, Grades, Money, Social Life)
type Card = (String, Action, Action)
type Resources = (Int, Int, Int, Int) -- (Sleep, Grades, Money, Social Life)
data CurrentKey = Left | Right | None deriving (Eq)
type State = (CurrentKey, Card, Resources, Week)
data Result = EndOfGame Week            -- end of game: week
            | ContinueGame State        -- continue with new state
-- type Game = Action -> State -> Result