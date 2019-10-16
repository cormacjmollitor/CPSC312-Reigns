module Types (Week, Action, Card, Resources, CurrentKey (Left, Right, None), State) where

type Week = Int
type Action = (Int, Int, Int, Int) -- (Sleep, Grades, Money, Social Life)
type Card = (String, Action, Action)
type Resources = (Int, Int, Int, Int) -- (Sleep, Grades, Money, Social Life)
data CurrentKey = Left | Right | None deriving (Eq)
type State = (CurrentKey, Card, Int, Resources, Week) -- Int is current deck index