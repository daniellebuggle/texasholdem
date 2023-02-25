{-https://github.com/jxwuu/blackjack-}
{-----------IMPORTS-----------}
import System.Random
import System.Exit (exitSuccess)
import System.IO
import Control.Monad
import Text.Read (readMaybe)
import Data.Maybe (fromJust)
import Data.Char (isDigit)
import Text.Read
import Debug.Trace
import Data.Array.IO
import GHC.IO.Unsafe
import Data.List
import Control.Exception (handle)
import GHC.Cmm.Dataflow (changedIf)

{-
This version of Texas Hold'em uses a previous Blackjack project as a base template -https://github.com/jxwuu/blackjack-.
Our version of Texas Hold'em implements the checking of all the possible hands. (Straight Flush, Four of a Kind, Full House, Flush,
Straight, Three of a Kind, Two Pair, Pair, High Card)
It determines the winner of either the player or the AI based off of these hands, whoever has the best one. 
-}


-- state consists of internal state, whether five cards are in the river or not, and a string of the winning hand
data State = State InternalState Bool String
         deriving (Ord, Eq, Show)

{-
internal state consists of a 5-tuple with values:
	- player's hand
	- CPU's hand
	- list of cards available to draw
  - cards of the river
	- boolean indicating current player (True = player, False = CPU)
-}
type InternalState = ([Card], [Card], [Card], [Card], Bool)

-- (suit, value (1 - 13, where ace = 1, jack = 11, queen = 12, king = 13)
type Card = (Char, Int)

type Player = State -> Action

type Game = Action -> State -> Result

-- result of a game is True if player won, False if computer won
data Result = EndOfGame Bool State
            | ContinueGame State
            | Debt State
            | Tie State
         deriving (Eq, Show)

{-
Actions:
		- Bet = allows the game to continue after either the player or the AI has placed a bet
-}
data Action = Bet
         deriving (Ord, Eq, Show)


{-----------GAME FUNCTIONS-----------}
texasholdem :: Game
-- if no available actions for either player, do nothing and continue the game
texasholdem Bet (State (pCards, cCards, deck, river, True) fiveCardsDrawn winningHand) =
    ContinueGame (State (pCards, cCards, deck, river, False)  fiveCardsDrawn winningHand)
texasholdem Bet (State (pCards, cCards, deck, river, False) fiveCardsDrawn winningHand) =
    ContinueGame (State (pCards, cCards, deck, river, True) fiveCardsDrawn winningHand)

-- | Draws a card from the deck and adds it to the river 
drawForRiver :: State -> State
drawForRiver (State (pCards, cCards, deck, river, currPlayer) fiveCardsDrawn winningHand) 
    | length river < 5 = (State (pCards, cCards, newDeckR, riverCard: river, currPlayer) fiveCardsDrawn winningHand)
    | otherwise = (State (pCards, cCards, newDeckR, river, currPlayer) (not fiveCardsDrawn) winningHand)
        where 
            (riverCard,newDeckR) = drawFromDeck deck 1

-- Randomly shuffles deck of cards
-- -- algorithm for shuffle taken from https://wiki.haskell.org/Random_shuffle
-- -- | Randomly shuffle a list
-- -- /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
    where
      n = length xs
      newArray :: Int -> [a] -> IO (IOArray Int a)
      newArray n xs =  newListArray (1,n) xs

-- Converts IO [Card] to [Card] for continued use in the game
processShuffledDeck :: IO [Card] -> [Card]
processShuffledDeck deck = unsafePerformIO deck

{-----------HELPER FUNCTIONS-----------}
-- | Draws 2 cards for each player and starts the river with 3 cards
startingDraw :: Int -> State -> State
startingDraw 0 (State (pCards, cCards, deck, river, currPlayer) fiveCardsDrawn winningHand) = State (pCards, cCards, deck, river, currPlayer)   fiveCardsDrawn winningHand
startingDraw n (State (pCards, cCards, deck, river, currPlayer) fiveCardsDrawn winningHand) 
  | n > 5 = startingDraw (n-1) (State (newCard:pCards, cCards, newDeck, river, currPlayer) fiveCardsDrawn winningHand)
  | n > 3 =  startingDraw (n-1) (State (pCards, newCard: cCards, newDeck, river, currPlayer) fiveCardsDrawn winningHand)
  | otherwise = startingDraw (n-1) (State (pCards, cCards, newDeck, newCard:river, currPlayer) fiveCardsDrawn winningHand)
    where 
        shuffleDeck = shuffle deck
        shuffledDeck = processShuffledDeck shuffleDeck 
        (newCard,newDeck) = drawFromDeck shuffledDeck 1

-- | Returns card drawn and new deck of cards (nth card is drawn from deck)
drawFromDeck :: [Card] -> Int -> (Card, [Card])
drawFromDeck deck n = (deck !! n, [card | card <- deck, card /= (deck !! n)])

-- | Adds up values of a list of cards
sumCards :: [Card] -> Int
sumCards [] = 0
sumCards ((_,val):tCard) = val + (sumCards tCard)


-- | Checks whether the ai or the player has the better hand
checkWinner:: State -> Result 
checkWinner (State (pCards, cCards, deck, river, currPlayer) fiveCardsDrawn winningHand)
    | compareHands pHand cHand == 1 = EndOfGame True (State (pCards, cCards, deck, river, currPlayer)   fiveCardsDrawn (fst pHand)) 
    | compareHands pHand cHand == 0 = EndOfGame False (State (pCards, cCards, deck, river, currPlayer)   fiveCardsDrawn (fst cHand))
    | otherwise = Tie (State (pCards, cCards, deck,river, currPlayer) fiveCardsDrawn "No winning Hand")
      where 
            pHandAndRiver = sortCardsAscending (concat[pCards, river])
            cHandAndRiver = sortCardsAscending (concat[cCards, river])
            pHand = checkHand pHandAndRiver pCards
            cHand = checkHand cHandAndRiver cCards


-- | Compares the hands of both the player and AI
--   Returns an Int for the winner - 1 for the player, 0 for the AI, 2 for a tie 
compareHands:: ([Char], Int) -> ([Char], Int) -> Int
compareHands pHand cHand 
  | royalFlush /= 3 = royalFlush
  | straightFlush /= 3 = straightFlush
  | fourOfAKind /= 3 = fourOfAKind
  | fullHouse /= 3 = fullHouse
  | flush /= 3 = flush
  | straight /=3 = straight
  | threeOfAKind /=3 = threeOfAKind
  | twoPair /= 3 = twoPair
  | pair /=3 = pair
  | otherwise = compareHandType "High Card" pHand cHand
   where royalFlush = compareHandType "Royal Flush" pHand cHand
         straightFlush = compareHandType "Straight Flush" pHand cHand
         fourOfAKind = compareHandType "Four of a Kind" pHand cHand
         fullHouse = compareHandType "Full House" pHand cHand
         flush = compareHandType "Flush" pHand cHand
         straight = compareHandType "Straight" pHand cHand
         threeOfAKind = compareHandType "Three of a Kind" pHand cHand
         twoPair = compareHandType "Two Pair" pHand cHand
         pair = compareHandType "Pair" pHand cHand

   
        
-- | Compares a certain hand - passing in the name of the hand 
--   Returns 1 if player has that hand, 0 if AI has that hand, 2 if they tie, 3 if neither has the hand
compareHandType:: String -> ([Char], Int) -> ([Char], Int) -> Int
compareHandType handType pHand cHand
  | fst pHand == handType && fst cHand == handType = compareCardValues (snd pHand) (snd cHand)
  | fst pHand == handType = 1
  | fst cHand == handType = 0
  | otherwise = 3


-- | Compare card number values 
--   Returns an Int for the higher card - 1 for the player, 0 for the AI, 2 for same value card
compareCardValues:: Int -> Int -> Int
compareCardValues pNumber cNumber 
  | pNumber > cNumber = 1
  | cNumber > pNumber = 0
  | otherwise = 2

-- | Checks what poker hand a player has
--   Returns tuple with string, name of the hand, and the highest card number in the hand
checkHand:: [Card] -> [Card] -> (String, Int)
checkHand handWRiver hand
  | snd royalFlush /= 0 = royalFlush
  | snd straightFlush /= 0 = straightFlush
  | snd fourOfAKind /= 0 = fourOfAKind
  | snd fullHouse /= 0 = fullHouse
  | snd flush /= 0 = flush
  | snd straight /= 0 = straight
  | snd threeOfAKind /= 0 = threeOfAKind
  | snd twoPair /= 0 = twoPair
  | snd pair /= 0 = pair
  | otherwise = checkHighCard hand 1
  where royalFlush = checkForRoyalFlush cardsBySuit
        straightFlush = checkForStraightFlush cardsBySuit
        fourOfAKind = checkForFour handWRiver
        fullHouse = checkForFullhouse handWRiver
        flush = checkForFlush handWRiver
        straight = checkForStraight handWRiver
        threeOfAKind = checkForThree handWRiver 
        twoPair = checkForTwoPair handWRiver
        pair = checkForPair handWRiver 0
        cardsBySuit = sortCardsBySuit handWRiver

-- | Sorts Cards based on their rank in ascending order
sortCardsAscending :: [Card] -> [Card]
sortCardsAscending hand = sortOn snd hand


-- | Compares two cards by their suit (i.e., by the first element of the tuple)
--
-- >>> sortCardsBySuit [('s', 1), ('d', 13), ('c', 10), ('d', 2), ('s', 2)]
-- [('c',10),('d',2),('d',13),('s',1),('s',2)]
compareCardsBySuit :: Card -> Card -> Ordering
compareCardsBySuit (suit1, rank1) (suit2, rank2)
  | suit1 == suit2 = compare rank1 rank2 -- If the suits are the same, compare the ranks
  | otherwise      = compare suit1 suit2 -- Otherwise, compare the suits

-- | Sorts a list of cards by suit
sortCardsBySuit :: [Card] -> [Card]
sortCardsBySuit = sortBy compareCardsBySuit

-- | Gets the highest value card from a list of cards
checkHighCard :: [Card] -> Int -> ([Char], Int)         
checkHighCard [] cardNum = ("High Card", cardNum)
checkHighCard (h:t) cardNum = if snd h > cardNum
                              then checkHighCard t (snd h)
                              else checkHighCard t cardNum

-- | Checks for a pair. If there are multiple pairs, higherPair determines whether the
--   higher pair or lower pair is returned
checkForPair :: [Card] -> Int -> ([Char], Int)       
checkForPair hand higherPair = if higherPair == 0                         
                         then case find (\x -> length x == 2) [filter (\c -> snd c == v) hand | v <- [1..13]] of
                               Just xs -> ("Pair", snd $ head xs)
                               Nothing -> ("Pair", 0)
                         else case find (\x -> length x == 2) [filter (\c -> snd c == v) hand | v <- reverse [1..13]] of
                               Just xs -> ("Pair", snd $ head xs)
                               Nothing -> ("Pair", 0)

-- | Checks for Two Pair. Uses checkForPair to make differentiate and find different pairs
checkForTwoPair :: [Card] -> ([Char], Int)
checkForTwoPair hand = if checkForPair hand 0 == checkForPair hand 1
                       then ("No Two Pair", 0)
                       else ("Two Pair", snd (checkForPair hand 0) + snd (checkForPair hand 1))

-- | Checks for 3 of a Kind.
checkForThree :: [Card] -> ([Char], Int)
checkForThree hand = case find (\x -> length x == 3) [filter (\c -> snd c == v) hand | v <- [1..13]] of
                      Just xs -> ("Three of a Kind", snd $ head xs)
                      Nothing -> ("Three of a Kind", 0)

-- | Checks for 4 of a Kind.                  
checkForFour :: [Card] -> ([Char], Int)
checkForFour hand = case find (\x -> length x == 4) [filter (\c -> snd c == v) hand | v <- [1..13]] of
                      Just xs -> ("Four of a Kind", snd $ head xs)
                      Nothing -> ("Four of a Kind", 0)  

-- | Checks for a Straight.
-- Need to sort by card value in ascending order using sortCardsAscending function before calling
checkForStraight :: [Card] -> ([Char], Int)          
checkForStraight (a:b:c:d:e:f:g:[])  
  | [snd c, snd d, snd e, snd f, snd g] == [snd c, snd c+1, snd c+2, snd c+3, snd c+4] = ("Straight", snd g)
  | [snd b, snd c, snd d, snd e, snd f] == [snd b, snd b+1, snd b+2, snd b+3, snd b+4] = ("Straight", snd f)
  | [snd a, snd b, snd c, snd d, snd e] == [snd a, snd a+1, snd a+2, snd a+3, snd a+4] = ("Straight", snd e)
  | otherwise = ("No straight", 0)
                    
-- | Checks for flush. Better flush is the one with the highest card
checkForFlush :: [Card] -> ([Char], Int)    
checkForFlush [] = ("No Flush", 0)
checkForFlush hand 
  | length diamonds == 5 = ("Flush", snd (checkHighCard diamonds 0))
  | length hearts == 5 = ("Flush", snd (checkHighCard hearts 0))
  | length clubs == 5 = ("Flush", snd (checkHighCard clubs 0))
  | length spades == 5 = ("Flush", snd (checkHighCard spades 0)) 
  | otherwise = ("No Flush", 0)
    where diamonds = filter (\c -> fst c == 'd') hand
          hearts = filter (\c -> fst c == 'h') hand
          clubs = filter (\c -> fst c == 'c') hand
          spades = filter (\c -> fst c == 's') hand

-- | Checks for a Full House. 
checkForFullhouse :: [Card] -> ([Char], Int)
checkForFullhouse hand 
  | (snd (checkForThree hand) /= snd (checkForPair hand 0)) && snd (checkForThree hand) /= 0 
    = ("Full House", snd (checkForThree hand) + snd (checkForPair hand 0))
  | (snd (checkForThree hand) /= snd (checkForPair hand 1) && snd (checkForThree hand) /= 0)
    = ("Full House", snd (checkForThree hand) + snd (checkForPair hand 0))
  | otherwise = ("No Full House", 0)

-- | Checks for a straight flush. Parameter list needs to be sorted by card suit
checkForStraightFlush :: [Card] -> ([Char], Int)        
checkForStraightFlush hand =
  case hand of
    (a:b:c:d:e:f:g:[]) ->
      if map snd [c, d, e, f, g] == [snd c, snd c+1, snd c+2, snd c+3, snd c+4] && (snd (checkForFlush [c,d,e,f,g]) == snd g)
        then ("Straight Flush", snd g)
        else if map snd [b, c, d, e, f] == [snd b, snd b+1, snd b+2, snd b+3, snd b+4] && (snd (checkForFlush [b,c,d,e,f]) == snd f)
          then ("Straight Flush", snd f)
          else if map snd [a, b, c, d, e] == [snd a, snd a+1, snd a+2, snd a+3, snd a+4] && (snd (checkForFlush [a,b,c,d,e]) == snd e)
            then ("Straight Flush", snd e)
            else ("No straight Flush", 0)
    _ -> ("Invalid input", 0)

-- | Checks for a Royal Flush. 
--   Royal Flush is Straight flush from 10 to Ace (in our case 9..13)
--   Check if Straight Flush with largest number as 13 then have Royal Flush
checkForRoyalFlush :: [Card] -> ([Char], Int)
checkForRoyalFlush hand 
  | snd (checkForStraightFlush hand) == 13 = ("Royal Flush", 13)
  | otherwise = ("No Royal Flush", 0)
-- | Averages the card values in the deck
avrg :: [Card] -> Int
avrg deck = sumCards deck `div` length deck


{-----------AI FUNCTIONS-----------}

-- ***this AI is inspired by the AI of a previous Blackjack project (https://wiki.ubc.ca/Course:CPSC312-2019/BlackjackSimulator);
-- this version makes a decision based on the current average of the deck (a real player could know this by taking into account the
-- hands of both players), and it also decides a certain bet amount to place in aiBet


-- takes in current state and average of the deck, will produce one of the following values 
--     0 == stand and don't bet
--     1 == stand and bet 
--     2 == draw and don't bet 
--     3 == draw and bet 
aiDecide :: (Ord a, Num a, Num p) => [Card] -> a -> p
aiDecide cHand pdecision 
      | (pdecision == 1) = 1
      | sumCards cHand > 1 = 2
      | otherwise = 3

-- requires checksum to be called before 
-- decides how much ai will bet based on aiDecide and how much money is currently in hand
-- returns the amount money ai will bet
aiBet :: [Card] -> Int -> Int -> Int -> Int
aiBet cHand avg money decision 
    | decision == 0 = 0 
    | decision == 2 = 0
    | decision == 1 && (sumCards cHand) `div` 21 * 100 > 66 = (sumCards cHand) * money `div` 100
    | decision == 1 &&  (sumCards cHand) `div` 21 * 100 <= 66 =  (sumCards cHand) * money `div` 200
    | otherwise = (sumCards cHand) * money `div` 300



{-----------CONSTANTS/STARTING STATES-----------}
-- whole 52 card deck
--	's' == spades
--	'd' == diamonds
--	'h' == hearts
--	'c' == clubs
fullDeck :: [Card]
fullDeck = [(suit,value) | suit <- ['s','d','h','c'], value <- [1..13]]

-- start of new texasholdem game
newGame :: State
newGame = State ([], [], fullDeck, [], True) False ""

{-----------USER INTERFACE-----------}

-- User's money, AI's money
type Bet = (Int, Int)

-- Start the game
start :: Game -> State -> IO Bet
start game state = 
    do
        putStrLn ("Game start! Welcome To Texas Hold'em. Please enter the amount of money that you would like to spend.")
        line <- getLine
        if (all isDigit line && (read line) >= 0) then
            play game state (read line :: Int, read line :: Int)
        else
            do
                putStrLn ("Please enter a non-negative integer.")
                start game state

-- Start the game (called by start)
play :: Game -> State -> Bet -> IO Bet
play game state (umoney, aimoney) =
  let newState = startingDraw 7 state
      (State (pCards, cCards, deck, river, currPlayer)   fiveCardsDrawn winningHand) = newState in
    do
      putStrLn("--------------------------\n");
      putStrLn("New game - Select whether to continue or quit:\n1 = quit \n2 = continue")
      line <- validInput
      if line == 1 then
        do
          putStrLn ("Done! Money Left - User: " ++ show umoney ++ " AI: " ++ show aimoney)
          return (umoney, aimoney)
      else 
          personPlay game (ContinueGame (State (pCards, cCards, deck, river, True) fiveCardsDrawn winningHand)) (umoney, aimoney) 0


-- Player IO handling function
--     1 == Check i.e. bet 0 
--     2 == Bet and choose how much
--     3 == Fold i.e. end current round and lose  
personPlay :: Game -> Result -> Bet -> Int -> IO Bet
personPlay game (ContinueGame state) (umoney, aimoney) value = 
    let (State (pCards, cCards, deck, river, currPlayer)   fiveCardsDrawn winningHand) = state in
      do
        putStrLn ("\nYour hand:         " ++ show pCards)
        putStrLn ("Computer's hand:   " ++ show cCards)
        putStrLn ("Current River      " ++ show river)
        putStrLn ("Your money:        " ++ show umoney)
        putStrLn ("Computer's money:  " ++ show aimoney)
        putStrLn ("Current pool:      " ++ show value)
        num <- randomRIO (0, (length deck) - 1) :: IO Int
        if fiveCardsDrawn then
          do 
            personPlay game (checkWinner state) (umoney, aimoney) value
        else 
          do
            putStrLn ("Do you want to check (1), bet(2) or fold(3)?")
            line <- validInput
            if line == 1 then
              do
                -- CHECK
                putStrLn("You chose to check.")
                -- so user bets 0
                aiPlay game (game (Bet) state) (umoney, aimoney) value 1 0 --`debug` ( show $ state)
            else if line == 2 then 
              -- BET
              do
                  putStrLn ("How much would you like to bet?")
                  line <- moneyHandle umoney
                  if (line /= -1) then
                    let x = line :: Int in
                    aiPlay game (game (Bet) (State (pCards, cCards, deck, river, currPlayer)   fiveCardsDrawn winningHand)) (umoney - x, aimoney) (x+value) 2 x--`debug` ( show $ state)
                  else
                    personPlay game (Debt state) (umoney, aimoney) value
            else 
              -- FOLD
              personPlay game (EndOfGame False state) (umoney, aimoney) value




personPlay game (EndOfGame player state) (umoney, aimoney) value = 
    let (State (pCards, cCards, deck, river, currPlayer)   fiveCardsDrawn winningHand) = state in
    do
        result <- updateBet state (umoney, aimoney) value player
        endOutput (state)
        play game newGame result

personPlay game (Tie state) (umoney, aimoney) value = 
    do
       putStrLn ("Tie!")
       endOutput (state)
       play game newGame (umoney, aimoney)

personPlay game (Debt state) (umoney, aimoney) value = 
    do
       putStrLn ("Bye! Until we meet again!")
       putStrLn ("-------------------------")
       start texasholdem newGame

-- AI IO handling function
aiPlay :: Game -> Result -> Bet -> Int -> Int -> Int -> IO Bet
aiPlay game (ContinueGame state) (umoney, aimoney) value pDecision pBet =
    let riverState = drawForRiver state
        (State (pCards, cCards, deck, river, currPlayer)   fiveCardsDrawn winningHand) = riverState in
    do
      let aiDecision = aiDecide cCards pDecision
      let computerBet = aiBet cCards (avrg deck) aimoney aiDecision 
      num <- randomRIO (0, (length deck) - 1) :: IO Int
      if aiDecision == 1 -- CHECK
        then
            do
              let computerBet = 0
              putStrLn ("AI bet: " ++ show computerBet)
              putStrLn("--------------------------");
              personPlay game (game (Bet) riverState) (umoney, aimoney) value
      else if aiDecision == 2 -- MATCH
        then
            do
              let computerBet = pBet
              putStrLn ("AI bet: " ++ show computerBet)
              putStrLn("--------------------------");
              personPlay game (game (Bet) riverState) (umoney, aimoney - computerBet) (value + computerBet) 
      else -- FOLD
         do
              aiPlay game (EndOfGame True riverState) (umoney, aimoney) value pDecision pBet
 
aiPlay game (EndOfGame player state) (umoney, aimoney) value pDecision pBet=
    do
       result <- updateBet state (umoney, aimoney) value player
       endOutput (state)
       putStrLn("--------------------------");
       play game newGame result

aiPlay game (Tie state) (umoney, aimoney) value pDecision pBet=
    do
       putStrLn ("Tie!")
       endOutput (state)
       putStrLn("--------------------------");
       play game newGame (umoney + (value `div` 2), aimoney + (value `div` 2))

-- updates the total bet pool and checks the state of the game
updateBet :: State -> Bet -> Int -> Bool -> IO Bet
updateBet (State (pCards, cCards, deck, river, currPlayer)   fiveCardsDrawn winningHand) (umoney, aimoney) value winner
   | winner = do 
      putStrLn("--------------------------\n");
      putStrLn ("You won!\n")
      x <- noMoney (umoney + value, aimoney);
      if(x > 1)
       then 
         start texasholdem newGame
      else 
       return (umoney + value, aimoney)
   | otherwise = do
      putStrLn("--------------------------\n");
      putStrLn ("AI won!\n")
      x <- noMoney (umoney, aimoney + value);
      if(x > 1)
       then 
         start texasholdem newGame
      else 
       return (umoney, aimoney + value)
  

-- prints out hand at the end of a game
endOutput :: State -> IO ()
endOutput (State (pCards, cCards, deck, river, currPlayer) fiveCardsDrawn winningHand) = 
    do 
        putStrLn ("Your hand:         " ++ show pCards)
        putStrLn ("Computer's hand:   " ++ show cCards)
        putStrLn ("Current River      " ++ show river)
        putStrLn ("\nWinning Hand:      " ++ show winningHand)


-- checks if you or the ai is out of money 
noMoney :: (Int,Int) -> IO Int
noMoney (umoney, aimoney) = do 
 if(umoney <= 0 || aimoney <= 0)
  then
   if(umoney <= 0)
    then 
     do 
      putStrLn "Oh no! You're out of money! I hope to see you again!"
      return 99
   else 
    do 
     putStrLn "You won! The AI is out of money"
     return 99
 else 
  do 
   putStrLn("--------------------------");
   return (-1)

-- checks if you have entered a valid amount of money for betting 
-- rejects non number, negative, and larger than money amount inputs 
moneyHandle :: Int -> IO Int
moneyHandle y = do
    input1 <- getLine
    case readMaybe input1 of
      Nothing -> do
        putStrLn "(integer input required, please try again)"
        moneyHandle y
      Just n -> do
        if(n <= y && n >= 0)
         then
          return n
        else 
         do
          putStrLn "Please enter a valid amount"
          moneyHandle y

validInput :: IO Int
validInput = do
  input1 <- getLine
  case readMaybe input1 of
    Nothing -> do
      putStrLn "(integer input required, please try again)"
      validInput
    Just n -> do
      if(n == 1 || n == 2 || n == 3)
       then
        return n
      else 
       do
        putStrLn "Please enter a valid option"
        validInput 
        

{-----------Start the program-----------}
--start texasholdem newGame