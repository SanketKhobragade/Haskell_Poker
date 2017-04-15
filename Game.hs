import InputDisplay 
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
		
game :: [Player] -> Int -> Int -> [Card]-> IO Int
game xs trn round cd = do
	if status (xs!!trn) == Fold then return (-1)
	else if status (xs!!trn) == Allin then return (-2)
	else if player_type (xs!!trn) == Human then do
		id <- (get_userid trn)
		display round trn xs cd
		let min_raise = 2*((round_bet (xs!!trn)) - (bet (xs!!trn)))
		let call = round_bet (maximumBy (compare `on` round_bet) xs)
		act <- link_action (action_decide xs trn) call min_raise (round_bet (xs!!trn))
		return act
	else do
		print(trn)
		return (cpu_decide xs trn round (show_cards cd round))
	

turn :: [Player] -> Int -> Int -> Int -> [Card] -> IO [Player]
turn p i n round cd = do
	print_head
	print_list (p) 0 
	if (n > 5 && (round_over p)) || (length (filter(\x -> status x == Play) p) == 1 && round_bet (p!!i) >= (round_bet (maximumBy (compare `on` round_bet) p))) then return p
	else do 
		bet <- game p i round  cd
		(turn (action p i bet) ((i+1) `mod` 5) (n+1) round) cd

round_game :: [Player] -> Int -> [Card] -> IO [Player]	
round_game p round cd = do
	if round > 3 then return p
	else do
		update1 <- turn p 0 1 round cd
		let update2 = clear_roundbet update1
		round_game update2 (round+1) cd
		
change :: [Player] -> [Player]
change xs = (tail xs) ++ [(head xs)]

blind :: [Player] -> [Player]
blind xs = (take 3 xs) ++ [small_blind(xs!!3)] ++ [big_blind(xs!!4)]  

small_blind :: Player -> Player
big_blind :: Player -> Player

small_blind xs = action_bet xs 20
big_blind xs = action_bet xs 40

start_game :: [Player]	-> IO [Player]
start_game p = do 
	let blind_list = blind p
	let shuffleList = shuffle c []
	let p_dist = dist_card (blind_list) shuffleList
	let community = comm_cards (drop 10 shuffleList)
	after_round <- round_game p_dist 0 community
	let best5list = hands after_round community
	let winnerlist = winning_player best5list
	let win = pot best5list
	let rem = foldl (\acc x -> if (status x == Fold) then (acc + 0) else (acc + 1)) 0 best5list
	if rem /= 1 then do
		putStrLn "Community cards"
		print(community)
		putStrLn "Winners"
		win_display(winnerlist)
	else print()
	let init = map (\x -> x{status = Play, round_bet = 0, bet = 0, cards = [], top5 = []}) win
	let chips_rem = map(\x -> chips x) init
	print(chips_rem)
	putStrLn "Press Enter to start new game"
	prompt <- getLine
	start_game (change init)

print_head :: IO ()
print_head = do
	putStrLn ("Name\tStatus\tChips\tBet\tRound Bet")

win_display :: [Player] -> IO ()
win_display [] = print()
win_display (x:xs) = do
	putStrLn ((show (name x)) ++ "\t" ++ (show (cards x)) ++"\t" ++ (show (drop 1 (top5 x))))
	win_display xs
	
	
print_list :: [Player] -> Int -> IO ()
print_list p i = if i<=4 then do
			putStrLn ((show (name pl)) ++ "\t" ++ (show (status pl)) ++"\t" ++ (show (chips pl)) ++ "\t" ++ (show (bet pl)) ++ "\t" ++ (show (round_bet pl)))
			print_list p (i+1)
		else putStrLn ""
		where pl = (p!!i)

main = do
	start_game (playerList 0 4)

	
