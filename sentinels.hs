data SList a b = Sent b | Cons (a, SList a b) deriving (Show,Eq)
