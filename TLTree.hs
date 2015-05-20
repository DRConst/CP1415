import Cp
type Tri = (Point, Side)
type Side = Int
type Point = (Int,Int)
data TLTree = Tri Tri | Nodo TLTree TLTree TLTree deriving (Show, Eq)

inTLTree :: Either Tri ((TLTree, TLTree), TLTree) -> TLTree
inTLTree = either Tri ((uncurry . uncurry) Nodo)

outTLTree :: TLTree -> Either Tri ((TLTree,TLTree),TLTree)
outTLTree (Nodo a b c)  = i2 ((a,b),c)
outTLTree (Tri a) = i1 a

recTLTree = id -|- id

--cataTLTree g = g . (recTLTree (cataTLTree g)) . outTLTree

--anaTLTree g = inTLTree . (recTLTree (anaTLTree g)) . g
