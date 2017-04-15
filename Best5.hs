module Best5 where

import Data.List (tails)
import Data.List
import Data.List (sortBy)
import Data.Function (on)
import System.Random

data Suit = Spade | Club | Diamond | Heart 
	deriving (Ord, Eq, Show)
data Card = Card {
	rank :: Int,
	suit :: Suit
	} deriving (Ord,Eq, Show)


group_by_rank :: [Card] -> [[Card]]
group_by_suit :: [Card] -> [[Card]]
sort_by_rank :: [Card] -> [Card]
sort_by_suit :: [Card] -> [Card]
sort_by_rank xs = sortBy (flip compare `on` rank) xs
sort_by_suit xs = sortBy (flip compare `on` suit) xs
group_by_rank xs = groupBy (\a b -> rank a == rank b) (sort_by_rank xs)
group_by_suit xs = groupBy (\a b -> suit a == suit b) (sort_by_suit xs)

----------Adding ace by changing its value since both (A K Q J 10) and (5 4 3 2 A) are strights -----
modify_straight1 :: [Card] -> [Card]
modify_straight2 :: [Card] -> [Card]
modify_straight1 (x:xs)
	| rank x == 13 = (x:xs) ++ [Card{rank = 0,suit = suit x}]
	| otherwise = x:xs
	
modify_straight2 xs = map (\x -> head x) (group_by_rank xs) 
	
-----------------------------------------------------------------------------------------------------

---------Finding flush by grouping on suit and taking highest length--------------

flush :: [Card] -> [Card]
flush [] = []
flush x = 
	sort_by_rank (maximumBy (compare `on` length) xs)
	where xs = group_by_suit x
	
---------------------------------------------------------------------------------------

---------Finding straight by checking for consecutive rank cards----------------------------
check_straight :: [Card] -> [Card]
check_straight [_] = []	
check_straight (x:y:xs) 
	| (rank x - rank y) == 1 && length xs == 0 = x : y : []
	| (rank x - rank y) == 1 && length xs /= 0 = x : check_straight (y:xs)
	| otherwise = [x]
	
straight :: [Card] -> [Card]
straight [] = []
straight (x:xs)
	| length (check_straight (x:xs)) >= 5 = check_straight (x:xs)
	| otherwise = straight xs
	
--------------------------------------------------------------------------------------------

--------Dividing cards into groups of same number, then taking 5 cards to ensure best possible comb.----

remove :: [Card] -> [[Card]] -> [[Card]]
remove card list = filter(\x -> x/=card) list

max_length :: [[Card]] -> [Card]
max_length [] = []
max_length (x:y:xs) = 
	if y == [] then x
	else 	if length x >= length y then x
		else y
	
	
sort_pairs :: [[Card]] -> [[Card]] -> Int -> [[Card]]
sort_pairs l1 l2 remain = 	
	if remain <= 0 then take 5 l2
	else if length (head (l1)) == remain then l2 ++ [head l1]
	else sort_pairs (remove xs l1) (l2 ++ [xs])  (remain - length xs)
	where 
	xs = maximumBy (compare `on` length) (reverse l1)

---------------------------------------------------------------------------------------------------------		
	
list_flush xs = flush xs
list_straight xs  =  (straight . sort_by_rank) ((modify_straight2 . modify_straight1) xs)
list_pair xs = sort_pairs  (group_by_rank xs) [] 5


compress :: [[Card]] -> [Card]
compress [] = []
compress (x:xs) = x ++ compress xs
 
------------------Finding best combination from 7 -------------------------------
preference :: (Ord a, Ord b, Num a, Num b) => [Card] -> [Card]
preference l = 
	if (length . list_straight . list_flush) l >= 5 then [Card{rank = 8, suit = Heart}] ++ take 5 ((list_straight . list_flush) l)
	else if (length ((list_pair l) !! 0 )) == 4 then [Card{rank = 7, suit = Heart}] ++ (compress . list_pair) l
	else if ((length ((list_pair l) !! 0 )) == 3 && (length ((list_pair l) !! 1 )) == 2) then [Card{rank = 6, suit = Heart}] ++  (compress . list_pair) l
	else if (length . list_flush) l >= 5 then [Card{rank = 5, suit = Heart}] ++ take 5 (list_flush l)
	else if (length . list_straight) l >= 5 then [Card{rank = 4, suit = Heart}] ++ take 5 (list_straight l)
	else if (length ((list_pair l) !! 0 )) == 3 then [Card{rank = 3, suit = Heart}] ++ (compress . list_pair) l
	else if ((length ((list_pair l) !! 0 )) == 2 && (length ((list_pair l) !! 1 )) == 2) then [Card{rank = 2, suit = Heart}] ++ (compress . list_pair) l
	else if (length ((list_pair l) !! 0 )) == 2 then [Card{rank = 1, suit = Heart}] ++ (compress . list_pair) l
	else [Card{rank = 0, suit = Heart}] ++ take 5 (sort_by_rank l)
	
-----------------------------------------------------------------------------

