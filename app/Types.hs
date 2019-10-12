module Types (Week, Action, Card, Resources, CurrentKey (Left, Right, None), State) where

type Week = Int
type Action = (Int, Int, Int, Int)
type Card = (String, Action, Action)
type Resources = (Int, Int, Int, Int)
data CurrentKey = Left | Right | None
type State = (CurrentKey, Card, Resources, Week)