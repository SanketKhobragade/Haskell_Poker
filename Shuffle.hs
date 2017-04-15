module Shuffle where

import System.IO.Unsafe                                         
import System.Random
import Player
import Best5
          
c :: Int
c = unsafePerformIO (getStdRandom (randomR (0,51)))  --Seed for shuffle function to start


---------------------Generating random number and adding to list if not present--------------
shuffle :: Int -> [Int] -> [Int]
shuffle ran xs = if (( elem ran xs) && (length xs < 15)) then shuffle random xs
			else if (length xs < 15) then shuffle random ([ran] ++ xs)
			else xs
			where random = unsafePerformIO (getStdRandom (randomR (0,51)))

-------------------Converting each integer to Data type Card---------------
int2card :: Int -> Card
int2card n = if n<=12 then Card{rank = n+1, suit = Spade}
		else if n<=25 then Card {rank = (mod n 13)+1 ,suit = Heart}
		else if n<=38 then Card {rank = (mod n 13)+1, suit = Club}
		else Card{rank = (mod n 13)+1 , suit = Diamond}
		

-----------------Initialising each player(Human/CPU) ----------------
init_player :: Int -> Int -> Player
init_player id val = if val< 0 then 
			Player{
				name = id ,
			 	status = Play,
			 	chips = start_money,
			 	bet = 0,
			 	round_bet = 0, 
			 	cards = [],
			 	top5 = [],
			 	player_type = Human
			}
		     else Player{
				name = id ,
			 	status = Play,
			 	chips = start_money,
			 	bet = 0,
			 	round_bet = 0, 
			 	cards = [],
			 	top5 = [],
			 	player_type = Cpu
			}


----------------Creating playerList--------------------------
playerList :: Int -> Int -> [Player]
playerList id val = if (id<5) then (init_player id (val-5)): (playerList (id+1) (val+1))
		else []	

dist_card :: [Player] -> [Int] -> [Player]
dist_card [] _ = []
dist_card (x:xs) (y1:y2:ys) = (x {cards = [int2card y1,int2card y2]}):(dist_card xs ys)
				
					
comm_cards ::[Int] -> [Card]
comm_cards (x:xs) = if xs==[] then [int2card x]
			else [int2card x] ++ comm_cards xs
			

show_cards :: [Card] -> Int -> [Card]
show_cards deck round = if round==0 then [] else
			if round == 1 then (take 3 deck)
			else if round == 2 then (take 4 deck)
			else deck 

start_money = 2000

player_list :: Int -> [Player] -> [Player]
player_list id xs = if (length xs) < 5 then player_list (id-1) ((init_player id 0):xs)
			else xs	 

