module InputDisplay where

import Graphics.UI.Gtk 			--Graphics library used for display
import Data.List
import Shuffle
import Best5
import Player
import System.IO.Unsafe
back = "deck/back.jpg"


display :: Int -> Int -> [Player] -> [Card] -> IO Int
display round turn p_list crd = do
  	initGUI
	window     <- windowNew		--Creating new window
	vbox       <- vBoxNew False 0
	
	set window [windowDefaultWidth := 600, windowDefaultHeight := 400,
		containerBorderWidth := 10,
	      windowTitle := "Texas Holdem Poker",
	      containerChild := vbox]
	
	label1     <- labelNew (Just "TABLE CARDS : ")
	miscSetAlignment label1 0 0
	boxPackStart vbox label1 PackNatural 0
	
	box1      <- makeCommunityCards crd round False 0 PackNatural 0		--Displaying community cards
	boxPackStart vbox box1 PackNatural 0
	
	sep        <- hSeparatorNew
	boxPackStart vbox sep PackNatural 10
	
	label2     <- labelNew (Just "PLAYER CARDS :")
	miscSetAlignment label2 0 0
	boxPackStart vbox label2 PackNatural 0
	
	box2       <- makePlayerCards (cards (p_list!!turn)) False 0 PackNatural 0 --Display player cards
	boxPackStart vbox box2 PackNatural 0
	
	sep2        <- hSeparatorNew
	boxPackStart vbox sep2 PackNatural 10
	
	
	
	quitbox    <- hBoxNew False 0
	boxPackStart vbox quitbox PackNatural 0
	quitbutton <- buttonNewWithLabel "Click on X to continue"
	boxPackStart quitbox quitbutton PackRepel 0
	widgetShowAll window
	--onClicked quitbutton mainQuit
	onDestroy window mainQuit
	mainGUI
	--num <- get_bet
	return 0
	
	
--------------Function to display each community card of each button----------------

makeCommunityCards :: [Card] -> Int -> Bool -> Int -> Packing -> Int -> IO HBox
makeCommunityCards xs round homogeneous spacing packing padding = do
	box     <- hBoxNew homogeneous spacing
	
	b 	<- check1 round 0 xs
	button1 <- buttonNew
	boxPackStart box button1 packing padding
	containerAdd button1 b
	
	b 	<- check1 round 1 xs
	button2 <- buttonNew
	boxPackStart box button2 packing padding
	containerAdd button2 b
	
	b 	<- check1 round 2 xs
	button3 <- buttonNew
	boxPackStart box button3 packing padding
	containerAdd button3 b
	
	b 	<- check2 round 3 xs
	button4 <- buttonNew
	boxPackStart box button4 packing padding
	containerAdd button4 b
	
	b 	<- check3 round 4 xs
	button5 <- buttonNew
	boxPackStart box button5 packing padding
	containerAdd button5 b
	
	return box
--------------------------------------------------------------

-----------Display Player's card ----------------------
	
makePlayerCards :: [Card] -> Bool -> Int -> Packing -> Int -> IO HBox
makePlayerCards xs homogeneous spacing packing padding = do
	box     <- hBoxNew homogeneous spacing
	b 	<- labelBox (card2file (xs!!0)) 
	button1 <- buttonNew
	boxPackEnd box button1 packing padding
	containerAdd button1 b
	
	b 	<- labelBox (card2file (xs!!1)) 
	button2 <- buttonNew
	boxPackEnd box button2 packing padding
	containerAdd button2 b
	
	return box
----------------------------------------------


--------------Conditions to show community cards---------------------
	
check1 :: Int -> Int -> [Card] -> IO HBox
check1 round i xs = if round /= 0 then labelBox (card2file (xs!!i))
			else labelBox back
			
check2 :: Int -> Int -> [Card] -> IO HBox
check2 round i xs = if round /= 1 && round /= 0 then labelBox (card2file (xs!!i))
			else labelBox back
			
check3 :: Int -> Int -> [Card] -> IO HBox
check3 round i xs = if (round /= 2 && round /=1 && round/=0) then labelBox (card2file (xs!!i))
			else labelBox back
	
----------------------------------------------------------------------------

-------------For uploading images -----------------------------
	
labelBox :: FilePath -> IO HBox
labelBox fn = do
	box <- hBoxNew False 0
	set box [ containerBorderWidth := 2]
	image <- imageNewFromFile fn
	boxPackStart box image PackNatural 3
	return box
	
card2file :: Card -> [Char]
card2file c = "deck/" ++ (show (rank c)) ++ (show (suit c)) ++ ".jpg"
---------------------------------------------------------------------------

--------------------Getting user input from user in console ------------------
get_action1 :: IO Int
get_action1 = do 
	putStrLn "1. Bet\n2. Check\n3. Fold "
	txt <- getLine
	let num = read txt :: Int
	r<- (get_bet1 num (-1) 0)
	return r

get_action2 :: Int -> Int -> IO Int
get_action2 x roundBet = do
	putStrLn ("1. Raise\n2. Call("++ show x ++")\n3. Fold")
	txt <- getLine
	let num = read txt :: Int
	r<- (get_bet1 num x roundBet ) 
	return r

get_action3 :: IO Int
get_action3 = do 
	putStrLn "1. All-in\n2. Fold "
	txt <- getLine
	let num = (read txt :: Int) 
	r<- (get_bet2 num)
	return r
	
{-get_action4 :: Int -> IO Int
get_action4 x = do
	putStrLn ("1. Allin\n2. Call("++ show x ++")\n3. Fold")
	txt <- getLine
	let num = read txt :: Int
	r<- (get_bet3 num x) 
	return r-}

get_bet :: Int -> Int -> IO Int
get_bet x roundBet = do 
	putStrLn ("Enter amount " ++ (show (2*(x-roundBet))) ++ " + ") 
	txt <- getLine
	let num = read txt :: Int
	return (num + (2*(x-roundBet)))
	
get_bet1 :: Int -> Int -> Int -> IO Int
get_bet1 x c roundBet = do
	if x == 3 then return (-1)
	else if x == 2 then return 0
	else if (c==(-1)) then (get_bet 0 0)
	else (get_bet c roundBet) 
	
get_bet2 :: Int -> IO Int
get_bet2 x = do 
	if x ==2 then return (-1)
	else return (-2)
	
get_bet3 :: Int -> Int -> IO Int
get_bet3 x c = do
	if x == 3 then return (-1)
	else if x == 2 then return 0
	else return (-2)
		
link_action :: Int -> Int -> Int -> Int ->IO Int --linking action according to round_bet and chips and bet
link_action x call y roundBet = do
	if x == 1 then get_action3 
	else if x == 2 then get_action1
	else (get_action2 call roundBet )
------------------------------------------------------------------------------	
	
