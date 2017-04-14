--import Poker 
import Data.List (tails)
import Data.List
import Data.List (sortBy)
import Data.Function (on)
import System.Random
import Shuffle
import Player
import Winner
import Best5
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
game :: [Player] -> Int -> Int -> [Card]-> IO Int
game xs trn round cd = do
	--print(trn)
	if status (xs!!trn) == Fold then return (-1)
	else if status (xs!!trn) == Allin then return (-2)
	else if player_type (xs!!trn) == Human then do
		--print(1)
		id <- (get_userid trn)
		display round trn xs cd
		--print(4)
		let min_raise = 2*((round_bet (xs!!trn)) - (bet (xs!!trn)))
		let call = round_bet (maximumBy (compare `on` round_bet) xs)
		--print(7)
		--print (round_bet (xs!!turn))
		act <- link_action (action_decide xs trn) call min_raise (round_bet (xs!!trn))
		--print(10)
		return act
	else do
		return (cpu_decide xs trn round cd)
	

turn :: [Player] -> Int -> Int -> Int -> [Card] -> IO [Player]
turn p i n round cd = do
	--print("Turn beg")
	print_head
	print_list (p) 0 
	--let x = map (\x -> round_bet x) p
	--print(round_over p)
	if (n > 5 && (round_over p)) || length (filter(\x -> status x == Play) p) == 1 then return p
	else do 
		bet <- game p i round  cd
		(turn (action p i bet) ((i+1) `mod` 5) (n+1) round) cd

round_game :: [Player] -> Int -> [Card] -> IO [Player]	
round_game p round cd = do
	if round > 3 then return p
	else do
		--print ("BeginRound")
		update1 <- turn p 0 1 round cd
		let update2 = clear_roundbet update1
		--print ("EndRound")
		round_game update2 (round+1) cd

start_game :: [Player]	-> IO [Player]
start_game p = do 
	let shuffleList = shuffle c []
	--x <- getLine
	--let l1 = (maximumBy (compare `on` length) ((group . sort ) (shuffleList)))
	let p_dist = dist_card (p) shuffleList
	let l1 = (map (\x -> cards x) p_dist)
	let community = comm_cards (drop 10 shuffleList)
	let l2 = l1 ++ [community]
	let l3 = (foldl (\acc x -> acc ++ x) [] l2)
	--print (l3 )
	after_round <- round_game p_dist 0 community
	let best5list = hands after_round community
	--print best5list
	let win = pot best5list 
	let init = map (\x -> x{status = Play, round_bet = 0, bet = 0, cards = [], top5 = []}) win
	--print init
	(start_game init)

print_head :: IO ()
print_head = do
	putStrLn ("Name\tStatus\tChips\tBet\tRound Bet")

print_list :: [Player] -> Int -> IO ()
print_list p i = if i<=4 then do
			putStrLn ((show (name pl)) ++ "\t" ++ (show (status pl)) ++"\t" ++ (show (chips pl)) ++ "\t" ++ (show (bet pl)) ++ "\t" ++ (show (round_bet pl)))
			print_list p (i+1)
		else putStrLn ""
		where pl = (p!!i)
			
	
print_winner :: [Player] -> Int -> IO()
print_winner x i = do
	if i < length ( winning_player x) then 
			print (top5 ( winning_player x)!!i)
			print_winner x i+1
	else putStrLn ""
		
main = do
	start_game (playerList 0 3)
	
