module Main where
import Data.List

isOrigin center rest = (foldl (+) 0 (map snd rest)) == (snd center)
complement :: [(String, Int)] -> (String, Int) -> Atoms
complement atoms x = if (head atoms) == x
                     then tail atoms
		     else (head atoms):(complement (tail atoms) x)
subtraction atoms [] = atoms		     
subtraction atoms part = if isInfixOf [head atoms] part
                         then subtraction (tail atoms) (complement part (head atoms))
			 else (head atoms):(subtraction (tail atoms) part)
nonOrigin atoms origin = Submol (complement atoms origin)		     
hasOrigin atoms = foldl (||) False (map (\c -> isOrigin c (complement atoms c)) atoms)
origin :: [(String, Int)] -> (String, Int)
origin atoms = fst $ head $ filter snd (map (\a -> (a, isOrigin a (complement atoms a))) atoms)
groupElms set = map (\ss -> (head ss, length ss)) (group (sort (map fst set)))
enumerateRows rows = [[(fst r, i) | i <- [0..l]] | r <- rows, let l = (snd r)]
decisionProduct xs ys = [y:x | x <- xs, y <- ys]
atomCountPermute set = foldl decisionProduct (map (\x -> [x]) (head groups)) (tail groups) 
                       where groups = (enumerateRows (groupElms set))
expandAtoms counts = foldl (++) [] (map (\c -> take (snd c) $ repeat (fst c)) counts)
dict key assocs = let (Just v) = lookup key assocs in v
subMols set = map ((\as -> map (\a -> (a, dict a elements)) as) . expandAtoms) (atomCountPermute set)
              where elements = set
isRGroup :: [(String, Int)] -> Int -> Bool
isRGroup atoms rCount = hasOrigin (("R", rCount):atoms)	      
findGroups :: [(String, Int)] -> ([(String, Int)] -> Bool) -> [[(String, Int)]]
findGroups atoms criterion = filter criterion (subMols atoms)
isR1Group atoms = isRGroup atoms 1
isR2Group atoms = isRGroup atoms 2
isR3Group atoms = isRGroup atoms 3
r1Groups atoms = findGroups atoms isR1Group
r2Groups atoms = findGroups atoms isR2Group
r3Groups atoms = findGroups atoms isR3Group
neutralizes part rest = (sum (map snd part)) == (sum (map snd rest))
data Molecule = Submol Atoms | Mol Atom Molecule Int deriving Show
type Atom = (String, Int)
type Atoms = [Atom]
branchSolve :: [(String, Int)] -> Molecule
branchSolve atoms = if (hasOrigin atoms)
                    then Mol (origin atoms) (nonOrigin atoms (origin atoms)) 0
		    else if (length (r1Groups atoms)) /= 0
		         then let solve = head $ filter (\g -> neutralizes g (subtraction atoms g)) (r1Groups atoms)
			          (Mol c (Submol m) b) = branchSolve (("R",1):solve)
			      in Mol c (Submol ((subtraction atoms (c:solve)) ++ m)) b
			 else Mol ("R",0) (Submol []) 0
main = putStrLn $ show $ branchSolve $ [("H", 1), ("O", 2), ("O", 2), ("H", 1)]
