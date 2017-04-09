import Graphics.UI.Gtk
import Data.List
import Shuffle

back = "deck/back.jpg"

pl_card = [Card{rank = 3,suit = Spade},Card {rank = 13,suit = Diamond} ]

display :: IO ()
display = do
  	initGUI
	window     <- windowNew
	vbox       <- vBoxNew False 0
	
	set window [windowDefaultWidth := 600, windowDefaultHeight := 400,
		containerBorderWidth := 10,
	      windowTitle := "Texas Holdem Poker",
	      containerChild := vbox]
	
	label1     <- labelNew (Just "COMMUNITY CARDS : ")
	miscSetAlignment label1 0 0
	boxPackStart vbox label1 PackNatural 0
	
	box1      <- makeCommunityCards community_cards 3 False 0 PackNatural 0
	boxPackStart vbox box1 PackNatural 0
	
	sep        <- hSeparatorNew
	boxPackStart vbox sep PackNatural 10
	
	label2     <- labelNew (Just "PLAYER CARDS :")
	miscSetAlignment label2 0 0
	boxPackStart vbox label2 PackNatural 0
	
	box2       <- makePlayerCards pl_card False 0 PackNatural 0
	boxPackStart vbox box2 PackNatural 0
	
	sep2        <- hSeparatorNew
	boxPackStart vbox sep2 PackNatural 10
	
	quitbox    <- hBoxNew False 0
	boxPackStart vbox quitbox PackNatural 0
	quitbutton <- buttonNewWithLabel "Quit"
	boxPackStart quitbox quitbutton PackRepel 0
	onClicked quitbutton mainQuit
	onDestroy window mainQuit
	widgetShowAll window
	mainGUI
	

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
	
	box6       <- actionButtons3 False 0 PackGrow 0
	boxPackEnd box box6 PackNatural 0
	
	return box
	
actionButtons1 :: Bool -> Int -> Packing -> Int -> IO VBox
actionButtons1 homogeneous spacing packing padding = do
	box     <- vBoxNew homogeneous spacing
	
	button3	<- buttonNewWithLabel "\tCall\t\t"
	button4	<- buttonNewWithLabel "\tRaise\t"
	button5	<- buttonNewWithLabel "\tFold\t\t"
--	button6	<- buttonNewWithLabel "\tAll in\t"
	
	boxPackStart box button3 packing padding
	boxPackStart box button4 packing padding
	boxPackStart box button5 packing padding
--	boxPackStart box button6 packing padding

	return box 

actionButtons2 :: Bool -> Int -> Packing -> Int -> IO VBox
actionButtons2 homogeneous spacing packing padding = do
	box     <- vBoxNew homogeneous spacing
	
--	button3	<- buttonNewWithLabel "\tCall\t\t"
	button4	<- buttonNewWithLabel "\tCheck\t"
	button5	<- buttonNewWithLabel "\tBet\t\t"
	button6	<- buttonNewWithLabel "\tAll in\t"
	
--	boxPackStart box button3 packing padding
	boxPackStart box button4 packing padding
	boxPackStart box button5 packing padding
	boxPackStart box button6 packing padding
	
	return box

actionButtons3 :: Bool -> Int -> Packing -> Int -> IO VBox
actionButtons3 homogeneous spacing packing padding = do
	box     <- vBoxNew homogeneous spacing
	
	button3	<- buttonNewWithLabel "\tCall\t\t"
--	button4	<- buttonNewWithLabel "\tRaise\t"
	button5	<- buttonNewWithLabel "\tFold\t\t"
	button6	<- buttonNewWithLabel "\tAll in\t"
	
	boxPackStart box button3 packing padding
--	boxPackStart box button4 packing padding
	boxPackStart box button5 packing padding
	boxPackStart box button6 packing padding
	
	return box
	
check1 :: Int -> Int -> [Card] -> IO HBox
check1 round i xs = if round /= 0 then labelBox (card2file (xs!!i))
			else labelBox back
			
check2 :: Int -> Int -> [Card] -> IO HBox
check2 round i xs = if round /= 1 && round /= 0 then labelBox (card2file (xs!!i))
			else labelBox back
			
check3 :: Int -> Int -> [Card] -> IO HBox
check3 round i xs = if (round /= 2 && round /=1 && round/=0) then labelBox (card2file (xs!!i))
			else labelBox back
	
labelBox :: FilePath -> IO HBox
labelBox fn = do
	box <- hBoxNew False 0
	set box [ containerBorderWidth := 2]
	image <- imageNewFromFile fn
	boxPackStart box image PackNatural 3
	return box
	
card2file :: Card -> [Char]
card2file c = "deck/" ++ (show (rank c)) ++ (show (suit c)) ++ ".jpg"

card = Card{rank = 2,suit = Spade}
	
main = display
