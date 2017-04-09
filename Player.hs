module Player where
import Best5 
import Data.List
import Data.Function (on)
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
	} deriving (Eq, Show)

func :: Player -> Int ->Player
func p round = 
	if (status p) /= Play then  p
	else if round == 0 then p
	else if round < 2 * (chips p) then p
	else p

--act1 :: Player -> Player
--act1 p = 

round_money :: [Player] -> Int
round_money xs =  (round_bet (head (sortBy (flip compare `on` round_bet) xs)))
	
action_bet :: Player-> Int ->Player
action_bet p money = 
	p{chips = chips p - money, bet = bet p + money, round_bet = round_bet p + money,
	 status = if chips p == money then Allin else Play}
	 
action_call :: Player-> Int ->Player
action_call p money = 
	p{chips = chips p - money, bet = bet p + money, round_bet = round_bet p + money}
	
action_raise :: Player-> Int ->Player	
action_raise p money = 
	p{chips = chips p - money, bet = bet p + money, round_bet = round_bet p + money,
	 status = if chips p == money then Allin else Play}	
	 
action_check p = p

action_fold p = p{status = Fold}

action_allin p = 
	p{bet = bet p + chips p, round_bet = round_bet p + chips p, chips = 0, status = Allin}

action :: [Player] -> Int -> [Player]
action p i = take (i-1) p ++ [func (p!!(i-1)) (round_money p)] ++ drop i p

round_over :: [Player] -> Bool
round_over p = 
	if length xs == 1 then True
	else False
	where xs = groupBy (\a b -> round_bet a == round_bet b || status a == Fold || status b == Fold || round_bet a < round_bet b && chips a == 0 || round_bet a > round_bet b && chips b == 0) p

turn :: [Player] -> Int -> Int -> [Player]
turn p i n = 
	if n > 5 && (round_over p) then p
	else turn (action p i) (i `mod` 5 + 1) (n+1)
	
--round :: [Player] -> Int -> Int

