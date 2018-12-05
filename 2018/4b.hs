-- --- Day 4: Repose Record ---

--- Part Two ---
-- Strategy 2: Of all guards, which guard is most frequently asleep on the same minute?
-- In the example above, Guard #99 spent minute 45 asleep more than any other guard or minute - three times in total. (In all other cases, any guard spent any minute asleep at most twice.)
-- What is the ID of the guard you chose multiplied by the minute you chose? (In the above example, the answer would be 99 * 45 = 4455.)


import Data.List.Split
import Data.List

getId :: String -> String
getId ('G' : 'u' : 'a' : 'r' : 'd' : ' ' : '#' : restOfString) =  head ( splitOn " " restOfString)
getId xs = xs

isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

parse :: String -> (Integer, String)
parse s =  
    let x = splitOn "] " s
        y:xs:[] = x
        time = read (y!!15:y!!16:[]) :: Integer
    in (time, getId xs)

reduce :: [(Integer, String, String)] -> [(String, [Integer])]
reduce ((start, id, _):(stop, _, _):xs) = (id, [start..stop-1]) : reduce xs
reduce (x:[]) = []
reduce [] = []

main = interact sumLines 

sumLines :: String -> String  
sumLines input =   
    let allLines = lines input
        x = map parse (sort allLines)
        y = snd ( mapAccumL (\a (b, c) -> if isNumber c then (c, (b, c, c)) else (a, (b, a, c))) "" x )
        w = filter (\(_,a,b) -> a /= b) y
        z = groupBy (\(_,a2,_) (_,b2,_) -> a2 == b2) (sortBy (\(_,a2,_) (_,b2,_) -> compare a2 b2) w)
        z1 = map (reduce) z
        z2 = map (\x -> foldr (\(id, range) (_, list) -> (id, range ++ list)) ("", []) x) z1
        z3 = map (\(id, list) -> (id, (maximum . map (\xs -> (length xs, head xs)) . group . sort) list)) z2
        (id, (rep, min)) = maximumBy (\(_, (r,_)) (_, (p,_)) -> compare r p) z3 
    in  "ID: " ++ show id ++ " Min:" ++ show min