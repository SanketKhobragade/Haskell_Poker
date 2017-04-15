module Player where
import Best5 
import Data.List
import Data.Function (on)

data User = Cpu | Human 
	deriving (Eq,Show)
data State = Play | Allin | Fold
	deriving (Eq, Show)
data Player = Player
	{
	name :: Int
	, status :: State
	, chips :: Int
	, bet :: Int
	, round_bet :: Int
	, cards :: [Card] 
	, top5 :: [Card]
	, player_type :: User
	} deriving (Eq, Show)

func :: Player -> Int -> Int -> Player
func p bet round = 
	if bet == 0 then action_call p (round - round_bet p)
	else if bet == (-1) then action_fold p 
	else if bet == (-2) then action_allin p
	else action_bet p bet

round_money :: [Player] -> Int
round_money xs =  (round_bet (head (sortBy (flip compare `on` round_bet) xs)))


	
action_bet :: Player-> Int ->Player
action_bet p money = 
	if money >= chips p then p{bet = bet p + chips p, round_bet = round_bet p + chips p, chips = 0, status = Allin}
	else p{chips = chips p - money, bet = bet p + money, round_bet = round_bet p + money}
	 
action_call :: Player -> Int ->Player
action_call p money = 
	p{chips = chips p - money, bet = bet p + money, round_bet = round_bet p + money}	

action_fold p = p{status = Fold}

action_allin p = 
	p{bet = bet p + chips p, round_bet = round_bet p + chips p, chips = 0, status = Allin}

action :: [Player] -> Int -> Int -> [Player]
action p i bet = take i p ++ [func (p!!i) (bet) (round_money p)] ++ drop (i+1) p

round_over :: [Player] -> Bool
round_over p = 
	if length xs == 0 then True
	else False
	where 
	xs = filter (\x -> round_bet x < potmoney && status x == Play && chips x > 0) p
	potmoney = round_bet (maximumBy (compare `on` round_bet) p)

clear_roundbet :: [Player] -> [Player]
clear_roundbet [] = []
clear_roundbet (x:xs) = 
	x{round_bet = 0} : clear_roundbet(xs)
	
	
action_decide :: [Player] -> Int -> Int
action_decide x turn = 
	if money_bet == round_money then 2
	else if (round_money - money_bet) >= chips pl then 1
	else if 2*(round_money - money_bet) >= chips pl then 4
	else 3
	where 
	pl = (x!!turn)
	money_bet = round_bet pl
	round_money = round_bet (maximumBy (compare `on` round_bet) x) 
	


