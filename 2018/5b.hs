--- Day 5: Alchemical Reduction ---

-- You've managed to sneak in to the prototype suit manufacturing lab. The Elves are making decent progress, but are still struggling with the suit's size reduction capabilities.
-- While the very latest in 1518 alchemical technology might have solved their problem eventually, you can do better. You scan the chemical composition of the suit's material and discover that it is formed by extremely long polymers (one of which is available as your puzzle input).
-- The polymer is formed by smaller units which, when triggered, react with each other such that two adjacent units of the same type and opposite polarity are destroyed. Units' types are represented by letters; units' polarity is represented by capitalization. For instance, r and R are units with the same type but opposite polarity, whereas r and s are entirely different types and do not react.

-- For example:

--     In aA, a and A react, leaving nothing behind.
--     In abBA, bB destroys itself, leaving aA. As above, this then destroys itself, leaving nothing.
--     In abAB, no two adjacent units are of the same type, and so nothing happens.
--     In aabAAB, even though aa and AA are of the same type, their polarities match, and so nothing happens.

-- Now, consider a larger example, dabAcCaCBAcCcaDA:

-- dabAcCaCBAcCcaDA  The first 'cC' is removed.
-- dabAaCBAcCcaDA    This creates 'Aa', which is removed.
-- dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
-- dabCBAcaDA        No further actions can be taken.

-- After all possible reactions, the resulting polymer contains 10 units.

import Data.Char
import Data.List
import Debug.Trace
import Data.Set (toList, fromList)

-- main = print ((map (\x -> (alchemicalReduction x)) ["aA", "abBA", "abAB", "aabAAB", "dabAcCaCBAcCcaDA"]) \\ ["", "", "abAB", "aabAAB", "dabCBAcaDA"])

main = interact (show . findMinumum) 

-- main = print (findMinumum "dabAcCaCBAcCcaDA") 

findMinumum :: String -> Int
findMinumum xs = (minimum (map (\x -> polymers (removePolymer xs x) ) (toList (fromList (alchemicalReduction xs)))))

removePolymer :: String -> Char -> String
removePolymer xs p = filter (not . (`elem` p : (toUpper p) : [])) xs

opposite :: Char -> Char -> Bool
opposite x y = (toUpper x == toUpper y) && (x /= y)

polymers :: String -> Int
polymers s = length (alchemicalReduction s)

alchemicalReduction :: String -> String  
alchemicalReduction = foldr r ""
    where
        r x (y:z) | opposite x y = z
        r x y = x : y


-- alchemicalReduction "" = ""
-- alchemicalReduction (x:[]) = [x]
-- alchemicalReduction (x:rest) = 
--     let reducted = alchemicalReduction(rest) in  
--         if (reducted /= []) && (opposite x (head reducted))
--             then alchemicalReduction(tail reducted)
--             else x : reducted        
