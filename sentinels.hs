import Cp

data SList a b = Sent b | Cons (a, SList a b) deriving (Show,Eq)


outSList :: SList a b -> Either b (a, SList a b)
outSList (Sent b) = i1 b
outSList (Cons( x , y )) = i2 (x, y)


inSList :: Either b (a, SList a b) -> SList a b 
inSList = either (Sent) (Cons)

recSList f = id -|- (id >< f)

cataSList f = f . (recSList (cataSList f)) . outSList

anaSList f = inSList . (recSList ( anaSList f ) ) . f

hyloSList f g = cataSList f . anaSList g

mergeSList :: Ord a => ([a],[a]) -> [a]
mergeSList = hyloSList (either id cons) mgen


mgen :: Ord a => ([a], [a]) -> Either [a] (a, ([a],[a]))
mgen (a,[]) = i1 a
mgen ([],a) = i1 a
mgen (x:xs, y:ys)
	|x <= y = i2 (x, (xs, y:ys))
	|otherwise = i2 (y, (x:xs,ys))

