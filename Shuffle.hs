module Shuffle where

import System.IO.Unsafe                                         
import System.Random
import Player
import Best5
          
c :: Int
c = unsafePerformIO (getStdRandom (randomR (0,51)))

shuffle :: Int -> [Int] -> [Int]
shuffle ran xs = if (( elem ran xs) && (length xs < 15)) then shuffle (unsafePerformIO (getStdRandom (randomR (1,52)))) xs
			else if (length xs < 15) then shuffle (unsafePerformIO (getStdRandom (randomR (1,52)))) (ran:xs)
			else xs

int2card :: Int -> Card
int2card n = if n<=12 then Card{rank = n+1, suit = Spade}
		else if n<=25 then Card {rank = (mod n 13)+1 ,suit = Heart}
		else if n<=38 then Card {rank = (mod n 13)+1, suit = Club}
		else Card{rank = (mod n 13)+1 , suit = Diamond}
		
list_shuffle = shuffle c []

init_player :: Int -> Player
init_player id = Player{
			name = id ,
		 	status = Play,
		 	chips = start_money,
		 	bet = 0,
		 	round_bet = 0, 
		 	cards = [int2card (list_shuffle !! ((id-1)*2)),int2card (list_shuffle !! (((id-1)*2)+1)) ],
		 	top5 = []
		 }
	
players = player_list 5 []	

c_cards= drop 10 list_shuffle
					
comm_cards ::[Int] -> [Card]
comm_cards (x:xs) = if xs==[] then [int2card x]
			else [int2card x] ++ comm_cards xs
			
community_cards = comm_cards c_cards

show_cards :: Int -> [Card]
show_cards round = if round==0 then [] else
			if round == 1 then (take 3 community_cards)
			else if round == 2 then (take 4 community_cards)
			else community_cards 

start_money = 500

player_list :: Int -> [Player] -> [Player]
player_list id xs = if (length xs) < 5 then player_list (id-1) ((init_player id):xs)
			else xs	 

