module Main where
import Data.List

isOrigin center rest = (foldl (+) 0 (map snd rest)) == (snd center)
complement x set = filter (x /=) set
hasOrigin atoms = foldl (||) False (map (\c -> isOrigin c (complement c atoms)) atoms)
groupElms set = map (\ss -> (head ss, length ss)) (group (sort (map fst set)))
enumerateRows rows = [[(fst r, i) | i <- [0..l]] | r <- rows, let l = (snd r)]
-- | TODO: decisionProduct xs ys = 
subMols set = groups where groups = (enumerateRows (groupElms set))
main = putStrLn $ show $ enumerateRows $ groupElms [("C", 4), ("O", 2), ("O", 2)]
