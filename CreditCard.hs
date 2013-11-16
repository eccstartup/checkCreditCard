module CreditCard (check, main) where

import Data.Char

check number = ord (last number) - 48 == checknum
      where part1 = (map (\x -> (*) x) (init $ concat $ replicate 8 [2,1]))
            part2 = (map (\x -> ord x - 48) number)
            thestring = concatMap show $ zipWith (\x y -> x y) part1 part2
            thenum = foldl (\x y -> x + ord y -48) 0 thestring
            checknum = mod (0 - thenum) 10

main = do
     numbers <- getContents
     let num = head $ lines numbers
     print $ check num
     
