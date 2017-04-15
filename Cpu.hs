module Cpu where 
import Player
import Winner
import Best5
import Data.List
import Data.Function (on)

player_list = [  Player{name = 1, status = Play, chips = 10, bet = 120, round_bet = 5, cards = [Card{rank = 10, suit = Club}, Card{rank = 2, suit = Club}], top5 = []},
		 Player{name = 2, status = Fold, chips = 8, bet = 120, round_bet = 0, cards = [Card{rank = 3, suit = Heart}, Card{rank = 8, suit = Diamond}], top5 = []},
		 Player{name = 3, status = Play, chips = 10, bet = 120, round_bet = 5, cards = [Card{rank = 12, suit = Heart}, Card{rank = 2, suit = Heart}], top5 = []}, 
		 Player{name = 4, status = Play, chips = 10, bet = 120, round_bet = 5, cards = [Card{rank = 5, suit = Diamond}, Card{rank = 5, suit = Heart}], top5 = []}, 
		 Player{name = 5, status = Fold, chips = 10, bet = 100, round_bet = 5, cards = [Card{rank = 5, suit = Club}, Card{rank = 5, suit = Club}], top5 = []}]
		 
community_cards = [Card{rank = 5, suit = Club}, Card{rank = 10, suit = Diamond}, Card{rank = 7, suit = Spade}, Card{rank = 13, suit = Diamond}, Card{rank = 9, suit = Heart}]


hands2 = map (\x -> x{top5 = preference (cards x ++ community_cards)})
play_list = hands2 player_list

all_possible :: Player -> [Card] -> [[Card]] -> Int
all_possible _ _ [] = 0
all_possible cpu deck (x:xs) = 
	if rank_comparison cpu_cards user_cards 0 == GT then (all_possible cpu deck xs) + 1
	else (all_possible cpu deck xs)
	where 
	cpu_cards = preference(cards cpu ++ deck)
	user_cards = preference(x ++ deck)

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

bet_range :: [Player] -> Int -> Int -> [Card] -> Int
bet_range p index round deck = 
	if round == 0 then bet_range0 p index
	else if round == 1 || round == 2 then bet_range1 p index round deck nouts
	else (bet_range2 p index deck)
	where
	nouts = outs p index deck (map numtocard [1..52]) 
	
bet_range0 :: [Player] -> Int -> Int
bet_range1 :: [Player] -> Int -> Int -> [Card] -> Int -> Int
bet_range2 :: [Player] -> Int -> [Card] -> Int

bet_range0 p i = 
	if suit (card1) == suit (card2) || ((rank (card1) > 10 && rank (card2) > 10)) || (abs (rank (card1) - rank(card2)) > 0 && abs (rank (card1) - rank(card2)) < 3) then 100
	else if rank card1 > 10 || rank card2 > 10 || rank card1 == rank card2 || abs (rank (card1) - rank(card2)) < 5 then 50
	else 20
	where
	card1 = (cards (p!!i))!!0
	card2 = (cards (p!!i))!!1

bet_range1 p i round deck nouts = 
	if odds > 1 then max (potmoney `div` (odds-1)) (nouts*10)
	else chips (p!!i)
	where
	odds = (100 - (nouts * 4 `div` round)) `div` (nouts * 4 `div` round)
	potmoney = foldl (\acc x -> acc + bet x) 0 p
	
bet_range2 p i deck = 
	if ways < 30 then chips (p!!i)
	else if ways < 80 then 500
	else if ways < 150 then 100
	else if ways < 200 then 50
	else 0
	where
	ways = user_ways p i deck
	
bet_amt :: [Player] -> Int -> Int -> [Card] -> Int
call_amt :: [Player] -> Int -> Int -> [Card] -> Int
cpu_decide :: [Player] -> Int -> Int -> [Card] -> [Player]
bet_amt p index round deck = 	
	if round < 3 then min range (40*round + 20)
	else min range 200
	where 
	range = bet_range p index round deck
	
call_amt p index round deck = 
	if range > 2*call_val then (range - call_val)
	else if range > call_val then call_val
	else -1
	where
	call_val = round_bet (maximumBy (compare `on` round_bet) p) - round_bet (p!!index)
	max_val = chips (p!!index)
	range = bet_range p index round deck
	
cpu_decide p index round deck = 
	if diff == 0 then action p index bet
	else action p index call
	where
	diff = round_bet (maximumBy (compare `on` round_bet) p) - round_bet (p!!index)
	bet = bet_amt p index round deck
	call = call_amt p index round deck

	
	
{-bluff :: [Player] -> Int -> [Card] -> Int
bluff p i deck = 
	if potmoney < 300 then 0
	else if elem (rank last_card) (map (\x -> rank x) (take (length deck - 1) deck)) && all_possible (p!!i){cards = [last_card]} deck (combtocards (combinations 2 list)) < 80 then 200
	else if length flushlist == 3 && elem last_card flushlist && nopair == 2 then 200
	else 0
	where 
	last_card = (reverse deck) !! 0
	flushlist = maximumBy (compare `on` length) (group_by_suit deck)
	nopair = length (maximumBy (compare `on` length) (group_by_rank deck))
	list = [1..52]\\cardtonum((cards (p!!i) ++ deck))
	potmoney = foldl (\acc x -> acc + bet x) 0 p-}
	
{- When to bluff (only when there is high money in pot)
1. When a card opens such that there is 3 cards of same colour
2. When at the end a number opens such that it was 2nd of same rank and is < 10 (Make sure flush or straight is not possible)
-}

--main = print(bet_amt play_list 1 3 community_cards)
--main = print(bluff play_list 1 community_cards)
--main = print(maximumBy (compare `on` length) (group_by_rank community_cards))



