import Cp
import LTree
--depth :: LTree c -> either a (b,b)
--depth = (either (const 1) (uncurry (max)) . (recLTree (either (const 1) max))  .  outLTree

--let t = Fork (Fork (Leaf 10, Fork (Leaf 2, Fork (Leaf 5, Leaf 3))), Leaf 23)
balanced (Leaf _) = True
balanced (Fork (t, t0)) = balanced t && balanced t0 && abs (depth t - depth t0) <=  1
depth = cataLTree (either (const 1) (succ.(uncurry max)))

