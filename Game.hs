import Poker 
import Data.List (tails)
import Data.List
import Data.List (sortBy)
import Data.Function (on)
import System.Random
import Shuffle
import Player
import Winner
import Cpu

get_userid ::Int ->  IO Int
get_userid turn = do 
	putStrLn "Enter user ID :"
	n<-getLine
	let id = read n :: Int  
	if id == turn  then return id
	else do
		putStrLn "Incorrect UserID"
		(get_userid turn)
		
-- Game function ReturnValue Type
game :: [Player] -> Int -> Int -> IO Int
game xs turn round = do
	print(turn)
	if status (xs!!turn) == Fold then return (-1)
	else if status (xs!!turn) == Allin then return (-2)
	else do
		--print(1)
		id <- (get_userid turn)
		display round turn xs
		--print(4)
		let min_raise = 2*((round_bet (xs!!turn)) - (bet (xs!!turn)))
		let call = round_bet (maximumBy (compare `on` round_bet) xs)
		--print(7)
		--print (round_bet (xs!!turn))
		act <- link_action (action_decide xs turn) call min_raise (round_bet (xs!!turn))
		--print(10)
		return act
	

turn :: [Player] -> Int -> Int -> Int -> IO [Player]
turn p i n round = do
	print("Turn beg")
	--let x = map (\x -> round_bet x) p
	print(round_over p)
	if (n > 5 && (round_over p)) || length (filter(\x -> status x == Play) p) == 1 then return p
	else do 
		bet <- game p i round
		(turn (action p i bet) ((i+1) `mod` 5) (n+1) round)

round_game :: [Player] -> Int -> IO [Player]	
round_game p round = do
	if round > 3 then return p
	else do
		print ("BeginRound")
		update1 <- turn p 0 1 round
		let update2 = clear_roundbet update1
		print ("EndRound")
		round_game update2 (round+1)
		
start_game :: [Player]	-> IO [Player]
start_game p = do 
	let p_dist = dist_card (playerList 0) (shuffle c [])
	let community = comm_cards (drop 10 (shuffle c []))
	after_round <- round_game p_dist 0
	let best5list = hands after_round community
	--print best5list
	let win = pot best5list 
	let init = map (\x -> x{status = Play, round_bet = 0, bet = 0, cards = [], top5 = []}) win
	print init
	(start_game init)
	
main = do 
	x <- start_game (playerList 0)
	print(minimum_bidder players_list)
	--print(round_over players_list)
	
