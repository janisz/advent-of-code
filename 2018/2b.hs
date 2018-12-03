-- --- Part Two ---

-- Confident that your list of box IDs is complete, you're ready to find the boxes full of prototype fabric.

-- The boxes will have IDs which differ by exactly one character at the same position in both strings. For example, given the following box IDs:

-- abcde
-- fghij
-- klmno
-- pqrst
-- fguij
-- axcye
-- wvxyz

-- The IDs abcde and axcye are close, but they differ by two characters (the second and fourth). However, the IDs fghij and fguij differ by exactly one character, the third (h and u). Those must be the correct boxes.

-- What letters are common between the two correct box IDs? (In the example above, this is found by removing the differing character from either ID, producing fgij.)



import Data.List

main = interact sumLines  

cartProd xs ys = [(x,y) | x <- xs, y <- ys]

calcP :: (String, String) -> (Int,String)
calcP ((x:xs), (y:ys))
  | x == y = append y (calcP (xs, ys))
  | otherwise = skip (calcP (xs, ys))
  where
    append y (index, results) = (index + 1, y : results)
    skip (index, results) = (index + 1, results)

calcP ("",x) = (0, x)
calcP (x,"") = (0, x)

sumLines :: String -> String  
sumLines input =   
    let allLines = lines input
        x = cartProd allLines allLines
        y = map calcP x
    in  snd (head (dropWhile (\(a,b) -> length b /= a-1) y))