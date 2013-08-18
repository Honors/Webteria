module Main where
import Data.List

isOrigin center rest = (foldl (+) 0 (map snd rest)) == (snd center)
complement x set = filter (x /=) set
hasOrigin atoms = foldl (||) False (map (\c -> isOrigin c (complement c atoms)) atoms)
groupElms set = map (\ss -> (head ss, length ss)) (group (sort (map fst set)))
main = putStrLn $ show $ groupElms [("C", 4), ("O", 2), ("O", 2)]
