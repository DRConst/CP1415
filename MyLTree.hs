import Cp
import LTree

balanced :: LTree a -> Bool
balanced (Leaf _) = True
balanced (Fork (t, t0)) = balanced t && balanced t0 && abs (depth t - depth t0) <=  1

depth :: LTree a -> Int
depth = cataLTree (either (const 1) (succ.(uncurry max)))

balance :: LTree a -> LTree a
balance = anaLTree lsplit . tips




