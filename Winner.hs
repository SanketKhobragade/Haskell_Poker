module Winner where

import Data.List (tails)
import Data.List
import Data.List (sortBy)
import Data.Function (on)
import Player
import Best5
import Shuffle

hands :: [Player] -> [Card] -> [Player]
hands xs deck = map (\x -> x{top5 = preference (cards x ++ deck)}) xs

------------------Finding which player(NOT folded) has best cards----------------------	

rank_comparison :: [Card] -> [Card] -> Int -> Ordering
rank_comparison a b i = 
	if rank (a!!i) > rank (b!!i) then LT
	else if rank (a!!i) < rank (b!!i) then GT
	else if i==5 then EQ
	else rank_comparison a b (i+1)

highest_cards :: [Player] -> [Player]
highest_cards [x] = [x]
highest_cards (x:xs) = 
	if rank_comparison (top5 x) (top5 (head max_cards)) 0 == LT then [x]
	else if rank_comparison (top5 x) (top5 (head max_cards)) 0 == GT then max_cards
	else [x] ++ max_cards
   	where max_cards = highest_cards xs
   	
playing xs = filter (\x -> status x /= Fold) xs

winning_player xs = (highest_cards . playing) xs

--------------------------------------------------------------------------------------

----------------Dividing game into pots and finding minimum pot money------------------
sort_by_bet :: [Player] -> [Player]
minimum_bidder :: [Player] -> Int
sort_by_bet xs = sortBy (compare `on` bet) xs
minimum_bidder (x : xs) = 
	if status x /= Fold then bet x
	else minimum_bidder xs
	
pot_money :: [Player] -> Int -> Int
pot_money xs money = foldl (\acc x -> if bet x <= money then acc + bet x else acc + money) 0 xs

---------------------------------------------------------------------------------------

---------------Finding winner of pot and giving chips to winner------------------------
modify :: Int -> Int -> [Player] -> [Player]
modify mini pot x = 
	modify_status
	where 
	winner = winning_player x
	modify_chips = map(\xs -> xs{chips = if (elem xs winner) then chips xs + pot `div` (length winner) else chips xs }) x
	modify_bet = map(\xs -> xs{bet = if bet xs < mini then 0 else bet xs - mini}) modify_chips
	modify_status = map(\xs -> xs{status = if bet xs == 0 then Fold else status xs}) modify_bet 
	
----------------------------------------------------------------------------------------

-------------Dividing game into pots and calling above function to find winner----------
	
	   	
pot :: [Player] -> [Player]
pot x = 
	modify minimum_bid money_in_pot x
	where 
	minimum_bid = (minimum_bidder . sort_by_bet) x
	money_in_pot = pot_money x minimum_bid

------------------------------------------------------------------------------------------	 

--main = print(map(\x -> chips x) (pot play_list))





