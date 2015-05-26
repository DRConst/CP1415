import Cp
import X3d
import System.Process
type Tri = (Point, Side)
type Side = Int
type Point = (Int,Int)
data TLTree = Tri Tri | Nodo ((TLTree, TLTree), TLTree) deriving (Show, Eq)

tri = Tri ((0,0),256)
n = Nodo ((tri, tri), tri)
t = Nodo ((n, n), tri)

inTLTree = either Tri Nodo

outTLTree :: TLTree -> Either Tri ((TLTree,TLTree),TLTree)
outTLTree (Nodo a)  = i2 (a)
outTLTree (Tri a) = i1 a

recTLTree f = id -|- ((f >< f) >< f)

cataTLTree g = g . (recTLTree (cataTLTree g)) . outTLTree

anaTLTree g = inTLTree . (recTLTree (anaTLTree g)) . g

hyloTLTree a b = cataTLTree a . anaTLTree b

tipsTLTree = cataTLTree (either singl conc)
		where conc((l1,l2),r) = l1 ++ l2 ++ r

auxSwap ((a,b),c) = ((c,b),a)

invTLTree = cataTLTree (inTLTree . ( id -|- auxSwap))

countTLTree = cataTLTree (either one add3)
		where add3 ((a,b),c) = a + b + c

maxP (a,b)
	| a > b = a
	| otherwise = b

depthTLTree = cataTLTree (either (const 1)  (succ . max3))
		where max3 ((a,b),c) 
			| c > maxP (a,b) = c
			| otherwise = maxP(a,b)

auxAna :: (Tri,Int) -> Either Tri (((Tri,Int),(Tri,Int)),(Tri,Int))
auxAna (a,0) = i1(a)
auxAna (((a,b),c),n) = i2((t1,t2),t3)       --i2 (((Tri((a,b),c2)), (Tri((a + c2, b),c2))), (Tri((a, b + c2),c2)))
			where c2 = c `quot` 2
			      n' = n-1
			      t1 = (((a,b),c2), n')
			      t2 = ((((a+c2),b),c2), n')
			      t3 = (((a,(b + c2)),c2), n')
curr :: Tri -> Int -> (Tri, Int)
curr a b = (a,b)

geraSierp :: (Tri,Int) -> TLTree
geraSierp = (anaTLTree auxAna)

apresentaSierp :: TLTree -> [Tri]
apresentaSierp = cataTLTree ( either singl conc3 )
			where conc3 ((a,b),c) = a ++ b ++ c



rep :: [Tri] -> String
rep [] = ""
rep (h:t) = (drawTriangle h) ++ rep t

render html = do {writeFile "_.html" html; system "open _.html"}

