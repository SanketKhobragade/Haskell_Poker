import Player
import Winner
import Best5
import Data.List

player_list = [  Player{name = 1, status = Play, chips = 10, bet = 20, round_bet = 5, cards = [Card{rank = 13, suit = Club}, Card{rank = 3, suit = Club}], top5 = []},
		 Player{name = 2, status = Fold, chips = 8, bet = 4, round_bet = 0, cards = [Card{rank = 3, suit = Heart}, Card{rank = 4, suit = Diamond}], top5 = []},
		 Player{name = 3, status = Play, chips = 10, bet = 15, round_bet = 5, cards = [Card{rank = 12, suit = Heart}, Card{rank = 2, suit = Heart}], top5 = []}, 
		 Player{name = 4, status = Play, chips = 10, bet = 20, round_bet = 5, cards = [Card{rank = 5, suit = Diamond}, Card{rank = 5, suit = Heart}], top5 = []}, 
		 Player{name = 5, status = Fold, chips = 10, bet = 20, round_bet = 5, cards = [Card{rank = 5, suit = Club}, Card{rank = 5, suit = Club}], top5 = []}]
		 
--community_cards = [Card{rank = 8, suit = Heart}, Card{rank = 13, suit = Diamond}, Card{rank = 12, suit = Club}, Card{rank = 12, suit = Spade}]-}


--hands = map (\x -> x{top5 = preference (cards x ++ community_cards)})
--play_list = hands player_list

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
	if user_ways p index (deck++[x]) < 100 && elem x (deck ++ cards (p!!index)) == False then outs p index deck xs + 1
	else outs p index deck xs

numtocard :: Int -> Card
numtocard x = 		 
	 if(x < 14) then Card{rank = x, suit = Spade} 
	 else if(x < 27) then Card{rank = x - 13, suit = Heart} 
	 else if(x < 40) then Card{rank = x - 26, suit = Diamond} 
	 else Card{rank = x - 39, suit = Club}
			 
combtocards xs = map(\x -> [(numtocard (x!!0)), (numtocard (x!!1))]) xs
main = print(outs play_list 0 community_cards (map numtocard [1..52]))


