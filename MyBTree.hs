import Cp
import BTree
--qsplit :: (Int, Int) ->either () (Int, ((Int, Int),(Int, Int)))

abpe (n,m) = anaBTree qsplit (n,m)

qsplit (a,b) 
	| b > a = i2 ((a `quot` 2) + (b `quot` 2) , ((a, (a `quot` 2) + (b `quot` 2) - 1 ),(((a `quot` 2) + (b `quot` 2) + 1), b)))
	| b == a = i2 (b, ((b, b - 1),((b + 1),b)))
	| otherwise = i1() 
