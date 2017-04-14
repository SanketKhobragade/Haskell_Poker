import Poker 
import Data.List (tails)
import Data.List
import Data.List (sortBy)
import Data.Function (on)
import System.Random
import Shuffle
import Player

get_userid ::Int ->  IO Int
get_userid turn = do 
	putStrLn "Enter user ID :"
	n<-getLine
	let id = read n :: Int  
	if id == turn  then return id
	else do
		putStrLn "Incorrect UserID"
		(get_userid turn)
		

game :: [Player] -> Int -> Int -> IO Int
game xs turn round = do 
	id <- (get_userid turn)
	display round turn xs
	let min_raise = 2*((round_bet (xs!!turn)) - (bet (xs!!turn)))
	let call = (round_bet (xs!!turn))
	
	
	act <- link_action (action_decide xs turn) call min_raise
	return act

main = do 
	pid <- game players 1 0 
	print (pid)
