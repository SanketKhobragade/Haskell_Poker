module Cpu where
import System.IO.Unsafe                                         
import System.Random 
import Player
import Winner
import Best5
import Data.List
import Data.Function (on)

----All possible ways cpu has opponent has better cards----
all_possible :: Player -> [Card] -> [[Card]] -> Int
all_possible _ _ [] = 0
all_possible cpu deck (x:xs) = 
	if rank_comparison cpu_cards user_cards 0 == GT then (all_possible cpu deck xs) + 1
	else (all_possible cpu deck xs)
	where 
	cpu_cards = preference(cards cpu ++ deck)
	user_cards = preference(x ++ deck)

--------combination of 2 cards from 52---------
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n xs = [ y:ys | y : xs' <- tails xs, ys <- combinations (n-1) xs']

cardtonum :: [Card] -> [Int]
cardtonum [] = []
cardtonum (x:xs) = 
	if (suit x) == Spade then [rank x] ++ cardtonum(xs)
	else if (suit x) == Heart then [rank x + 13] ++ cardtonum(xs)
	else if (suit x) == Diamond then [rank x + 26] ++ cardtonum(xs)
	else [rank x + 39] ++ cardtonum(xs)
			
user_ways :: [Player] -> Int -> [Card] -> Int
user_ways p index deck = 
	all_possible (player) deck (combtocards (combinations 2 list))
	where 
	player = p !! index
	list = [1..52]\\cardtonum((cards (p!!index) ++ deck))
	
---------No of cards which will be in favour of us-------------------
outs :: [Player] -> Int -> [Card] -> [Card] -> Int
outs _ _ _ [] = 0
outs p index deck (x:xs) = 
	if user_ways p index (deck++[x]) < 80 && elem x (deck ++ cards (p!!index)) == False then outs p index deck xs + 1
	else outs p index deck xs

numtocard :: Int -> Card
numtocard x = 		 
	 if(x < 14) then Card{rank = x, suit = Spade} 
	 else if(x < 27) then Card{rank = x - 13, suit = Heart} 
	 else if(x < 40) then Card{rank = x - 26, suit = Diamond} 
	 else Card{rank = x - 39, suit = Club}
			 
combtocards xs = map(\x -> [(numtocard (x!!0)), (numtocard (x!!1))]) xs

----------------Setting maximum range of bet-----------------
bet_range :: [Player] -> Int -> Int -> [Card] -> Int
bet_range p index round deck = 
	if round == 0 then bet_range0 p index
	else if round == 1 || round == 2 then max (bet_range1 p index round deck nouts) (10 * nouts)
	else (bet_range2 p index deck)
	where
	nouts = outs p index deck (map numtocard [1..52]) 
	

bet_range0 :: [Player] -> Int -> Int						-- round 0
bet_range1 :: [Player] -> Int -> Int -> [Card] -> Int -> Int			-- round 1
bet_range2 :: [Player] -> Int -> [Card] -> Int					-- round 2

bet_range0 p i = 
	if f == 1 then 0
	else if suit (card1) == suit (card2) || ((rank (card1) > 10 && rank (card2) > 10)) || (abs (rank (card1) - rank(card2)) > 0 && abs (rank (card1) - rank(card2)) < 3) then 100 -- 
	else if rank card1 > 10 || rank card2 > 10 || rank card1 == rank card2 || abs (rank (card1) - rank(card2)) < 5 then 50
	else 20
	where
	card1 = (cards (p!!i))!!0
	card2 = (cards (p!!i))!!1
	num = 200000000 - i	-- adding wait time
	f = last [1..num]

bet_range1 p i rnd deck nouts = 
	if odds > 1 then (ceiling mon)
	else chips (p!!i)
	where
	odds = (100 - (4 * ratio)) / (4 * ratio)
	potmoney = foldl (\acc x -> acc + bet x) 0 p
	fnouts = fromIntegral nouts :: Float
	fround = fromIntegral rnd :: Float
	ratio = fnouts / fround
	fpotmoney = fromIntegral potmoney :: Float
	mon = fpotmoney / (odds - 1)
	
bet_range2 p i deck = 
	if f == 1 then 0
	else if ways < 30 then chips (p!!i)
	else if ways < 80 then 500
	else if ways < 150 then 100
	else if ways < 200 then 50
	else 0
	where
	ways = user_ways p i deck
	num = 200000000 - i
	f = last [1..num]
	
bet_amt :: [Player] -> Int -> Int -> [Card] -> Int
call_amt :: [Player] -> Int -> Int -> [Card] -> Int
cpu_decide :: [Player] -> Int -> Int -> [Card] -> Int

-- Finding how much player player has to bet in his turn-----
bet_amt p index round deck = 	
	if round == 3 && range > 200 then 200
	else if round == 3 then 0
	else if round < 3 && range < 50 then 0
	else if round < 3 then (40*round + 20)
	else min range 200
	where 
	range = bet_range p index round deck
	
-- Finding how much cpu shoul call or raise --------
call_amt p index round deck = 
	if round == 3 && range > 200 then call_val + 200
	else if range > (2*call_val) && round < 3 && range > 2*call_val + round_bet (p!!index) then max (2*call_val + round_bet (p!!index)) (40*round + 20)
	else if range > call_val then call_val
	else (-1)
	where
	call_val = round_bet (maximumBy (compare `on` round_bet) p) - round_bet (p!!index)
	max_val = chips (p!!index)
	range = bet_range p index round deck
	
cpu_decide p index round deck = 
	if diff == 0 then min ((bet `div` 10) * 10) (bluff1 round) 
	else if call == (-1) then (-1)
	else (call `div` 10) * 10
	where
	diff = round_bet (maximumBy (compare `on` round_bet) p) - round_bet (p!!index)
	bet = bet_amt p index round deck
	call = call_amt p index round deck

-- Bluff--
bluff_or_not ::Int -> Bool
bluff_or_not r = if r<25 then True
			else False
		
bluff1 :: Int -> Int
bluff1 round = 
	if (bluff_or_not (unsafePerformIO (getStdRandom (randomR (0,99))))) then 0
	else 5000000

