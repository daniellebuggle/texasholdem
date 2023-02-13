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

-- state consists of internal state and whether a player can Hit again or not
data State = State InternalState Bool Bool
         deriving (Ord, Eq, Show)

{-
internal state consists of a 5-tuple with values:
	- player's hand
	- CPU's hand
	- list of cards available to draw
    - cards of the river
	- boolean indicating current player (True = player, False = CPU)
-}
--type InternalState = ([Card], [Card], [Card], Bool)
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
		- Hit n = take another card (of index n, to be used by an external RNG)
		- Stand = take no more cards for the round
-}
data Action = Hit Int
            | Stand
         deriving (Ord, Eq, Show)


{-----------GAME FUNCTIONS-----------}

blackjack :: Game
-- if no available actions for either player, do nothing and continue the game
blackjack act (State (pCards, cCards, deck, river, True) False cCanHit) =
    ContinueGame (State (pCards, cCards, deck, river, False) False cCanHit)
blackjack act (State (pCards, cCards, deck, river,  False) pCanHit False) =
    ContinueGame (State (pCards, cCards, deck, river, True) pCanHit False)

-- action = hit (needs to use checkSum to check the sum of a player's card values after a card was drawn,
--				 and drawFromDeck to draw a card from the deck)
blackjack (Hit n) (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit) 
    | currPlayer = checkSum (State (newCard:pCards, cCards, newDeck, river, not currPlayer) pCanHit cCanHit)
    | otherwise = checkSum (State (pCards, newCard:cCards, newDeck, river, not currPlayer) pCanHit cCanHit)
        where
             (newCard,newDeck) = drawFromDeck deck n

-- action = stand (sets player's boolean flag (pCanHit/cCanHit) to False)
blackjack (Stand) (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit)
    | pCanHit == cCanHit && cCanHit == False = checkSum (State (pCards, cCards, deck, river, currPlayer) False False)
    | pCanHit == False || cCanHit == False = checkSum (State (pCards, cCards, deck, river, currPlayer) False False)
    | currPlayer = ContinueGame (State (pCards, cCards, deck, river, not currPlayer) False cCanHit)
    | otherwise = ContinueGame (State (pCards, cCards, deck, river, not currPlayer) pCanHit False)

 

{-----------HELPER FUNCTIONS-----------}

-- returns card drawn and new deck of cards (nth card is drawn from deck)
drawFromDeck :: [Card] -> Int -> (Card, [Card])
drawFromDeck deck n = (deck !! n, [card | card <- deck, card /= (deck !! n)])

-- adds up values of a list of cards
sumCards :: [Card] -> Int
sumCards [] = 0
sumCards ((_,val):tCard) = val + (sumCards tCard)

-- checks whether a player has exceeded 21
checkSum :: State -> Result
checkSum (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit)
    | pSum > 21 || (not pCanHit && not cCanHit && (21 - pSum) > (21 - cSum)) = EndOfGame False (State (pCards, cCards, deck, river, currPlayer) False False)
    | cSum > 21 || (not pCanHit && not cCanHit && (21 - pSum) < (21 - cSum)) = EndOfGame True (State (pCards, cCards, deck, river, currPlayer) False False)
    | (not pCanHit && not cCanHit && (21 - pSum) == (21 - cSum)) = Tie (State (pCards, cCards, deck,river, currPlayer) pCanHit cCanHit)
    | otherwise = ContinueGame (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit)
      where
        pSum = sumCards pCards
        cSum = sumCards cCards

-- averages the card values in the deck
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
aiDecide :: (Ord a, Num a, Num p) => [Card] -> [Card] -> a -> p
aiDecide pHand cHand avg
    | (21 - fromIntegral (sumCards cHand)) < avg && ((sumCards pHand)-2) < (sumCards cHand)*4 `div` 3  = 1
    | (21 - fromIntegral (sumCards cHand)) < avg && ((sumCards pHand)-2) > (sumCards cHand) = 0
    | (21 - fromIntegral (sumCards cHand)) >= avg && (sumCards pHand) >= (sumCards cHand)*4 `div` 3 = 2
    | otherwise = 3

-- requires checksum to be called before 
-- decides how much ai will bet based on aiDecide and how much money is currently in hand
-- returns the amount money ai will bet
aiBet :: [Card] -> Int -> Int -> Int -> Int
aiBet cHand avg money decision 
    | decision == 0 = 0 
    | decision == 2 = 0
    | decision == 1 &&  (sumCards cHand) `div` 21 * 100 > 66 = (sumCards cHand) * money `div` 100
    | decision == 1 &&   (sumCards cHand) `div` 21 * 100 <= 66 =  (sumCards cHand) * money `div` 200
    | otherwise = (sumCards cHand) * money `div` 300


{-----------CONSTANTS/STARTING STATES-----------}
-- whole 52 card deck
--	's' == spades
--	'd' == diamonds
--	'h' == hearts
--	'c' == clubs
fullDeck :: [Card]
fullDeck = [(suit,value) | suit <- ['s','d','h','c'], value <- [1..13]]

-- start of new blackjack game
newGame :: State
newGame = State ([], [], fullDeck, [], True) True True

{-----------USER INTERFACE-----------}

-- User's money, AI's money
type Bet = (Int, Int)

-- Start the game
start :: Game -> State -> IO Bet
start game state = 
    do
        putStrLn ("Game start! Welcome To BlackJack. Please enter the amount of money that you want to spend.")
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
  let (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit) = state in
    do
      putStrLn("\n--------------------------\n");
      putStrLn ("New game - select who starts first:\n1 = quit, 2 = you, 3 = computer")
      line <- validInput
      --num <- randomRIO (0, (length deck) - 1) :: IO Int
      if line == 1 then
        do
          putStrLn ("Done! Money Left - User: " ++ show umoney ++ " AI: " ++ show aimoney)
          return (umoney, aimoney)
      else if line == 2 then 
          person_play game (ContinueGame (State (pCards, cCards, deck, river, True) pCanHit cCanHit)) (umoney, aimoney) 0
      else
          ai_play game (ContinueGame (State (pCards, cCards, deck, river, False) pCanHit cCanHit)) (umoney, aimoney) 0

-- Player IO handling function
--     0 == stand and don't bet
--     1 == stand and bet 
--     2 == draw and don't bet 
--     3 == draw and bet 
person_play :: Game -> Result -> Bet -> Int -> IO Bet
person_play game (ContinueGame state) (umoney, aimoney) value = 
    let (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit) = state in
      do
        putStrLn ("\nYour hand:         " ++ show pCards)
        putStrLn ("Computer's hand:   " ++ show cCards)
        putStrLn ("Your money:        " ++ show umoney)
        putStrLn ("Computer's money:  " ++ show aimoney)
        putStrLn ("Current pool:      " ++ show value)
        putStrLn ("Can you hit:       " ++ show pCanHit)
        putStrLn ("Can AI hit:        " ++ show cCanHit)
        num <- randomRIO (0, (length deck) - 1) :: IO Int
        if pCanHit == False then
            ai_play game (game (Stand) state) (umoney, aimoney) value
        else
            do
                putStrLn ("How much do you want to bet?")
                line <- moneyHandle umoney
                if (line /= -1) then
                    let x = line :: Int in
                        do
                            putStrLn ("\nChoose whether to hit (\"1\") or stand (any other key)")
                            line <- getLine
                            if line == "1" then 
                                --ai_play game (game (Hit 1) state) (umoney - x, aimoney) $x+value
                                ai_play game (game (Hit num) (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit)) (umoney - x, aimoney) $x+value --`debug` ( show $ state)
                            else
                                ai_play game (game (Stand) state) (umoney - x, aimoney) $x+value --`debug` ( show $ state)
                else
                    person_play game (Debt state) (umoney, aimoney) value

person_play game (EndOfGame player state) (umoney, aimoney) value = 
    let (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit) = state in
    do
        result <- update_bet state (umoney, aimoney) value
        endOutput (state)
        play game newGame result

person_play game (Tie state) (umoney, aimoney) value = 
    do
       putStrLn ("Tie!")
       endOutput (state)
       play game newGame (umoney, aimoney)

person_play game (Debt state) (umoney, aimoney) value = 
    do
       putStrLn ("Bye! Until we meet again!")
       putStrLn ("-------------------------")
       start blackjack newGame

-- AI IO handling function
ai_play :: Game -> Result -> Bet -> Int -> IO Bet
ai_play game (ContinueGame state) (umoney, aimoney) value =
    let (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit) = state in
    do
      let aiDecision = aiDecide pCards cCards (avrg deck)
      let computerBet = aiBet cCards (avrg deck) aimoney aiDecision 
      num <- randomRIO (0, (length deck) - 1) :: IO Int
      if aiDecision == 0 --stand and don't bet
        then
            do
              putStrLn ("AI stand but don't bet");
              putStrLn("--------------------------");
              person_play game (game Stand state) (umoney, aimoney) value
      else if aiDecision == 1 --stand and bet 
        then
            do
              putStrLn ("AI bet: " ++ show computerBet)
              putStrLn("--------------------------");
              person_play game (game Stand state) (umoney, aimoney - computerBet) (value + computerBet) 
      else if aiDecision == 2  -- draw and don't bet 
        then
            do
              putStrLn ("AI draw but don't bet");
              putStrLn("--------------------------");
              person_play game (game (Hit num) state) (umoney, aimoney) value 
      else --draw and bet
         do
              putStrLn ("AI bet: " ++ show computerBet)
              putStrLn("--------------------------");
              person_play game (game (Hit num) state) (umoney, aimoney - computerBet) (value + computerBet)
 
ai_play game (EndOfGame player state) (umoney, aimoney) value =
    do
       result <- update_bet state (umoney, aimoney) value
       endOutput (state)
       putStrLn("--------------------------");
       play game newGame result

ai_play game (Tie state) (umoney, aimoney) value =
    do
       putStrLn ("Tie!")
       endOutput (state)
       putStrLn("--------------------------");
       play game newGame (umoney + (value `div` 2), aimoney + (value `div` 2))

-- updates the total bet pool and checks the state of the game
update_bet :: State -> Bet -> Int -> IO Bet
update_bet (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit) (umoney, aimoney) value
   | (sumCards pCards) < 21 && (sumCards cCards) < (sumCards pCards) = do 
      putStrLn("--------------------------");
      putStrLn ("You won!")
      x <- noMoney (umoney + value, aimoney);
      if(x > 1)
       then 
         start blackjack newGame
      else 
       return (umoney + value, aimoney)
   | (sumCards cCards) < 21 && (sumCards cCards) > (sumCards pCards)  = do
      putStrLn("--------------------------");
      putStrLn ("AI won!")
      x <- noMoney (umoney, aimoney + value);
      if(x > 1)
       then 
         start blackjack newGame
      else 
       return (umoney, aimoney + value)
   | (sumCards pCards) > 21 && (sumCards cCards) > 21 = do
      putStrLn("--------------------------");
      putStrLn ("Both lose!")
      x <- noMoney (umoney, aimoney);
      if(x > 1)
       then 
         start blackjack newGame
      else 
       return (umoney, aimoney)
   | (sumCards pCards) == 21 = do
      putStrLn("--------------------------");
      putStrLn("You win!")
      x <- noMoney (umoney + value, aimoney);
      if(x > 1)
       then 
         start blackjack newGame
      else 
       return (umoney + value, aimoney) 
   | (sumCards cCards) == 21 = do
      putStrLn("--------------------------");
      putStrLn("AI won!")
      x <- noMoney (umoney, aimoney + value);
      if(x > 1)
       then 
         start blackjack newGame
      else 
       return (umoney, aimoney + value) 
   | (sumCards pCards) < 21 && (sumCards cCards) > 21 = do
      putStrLn("--------------------------");
      putStrLn("You Won")
      x <- noMoney (umoney + value, aimoney);
      if(x > 1)
       then 
         start blackjack newGame
      else 
       return (umoney + value, aimoney) 
   | (sumCards cCards) < 21 && (sumCards pCards) > 21 = do
      putStrLn("--------------------------");
      putStrLn("AI Won")
      x <- noMoney (umoney, aimoney+ value);
      if(x > 1)
       then 
         start blackjack newGame
      else 
       return (umoney, aimoney + value) 
   | otherwise = do 
      putStrLn("--------------------------");
      putStrLn("AI Won")
      x <- noMoney (umoney, aimoney + value);
      if(x > 1)
       then 
         start blackjack newGame
      else 
       return (umoney, aimoney + value) 

-- prints out hand at the end of a game
endOutput :: State -> IO ()
endOutput (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit) = 
    do 
        putStrLn ("Your hand:         " ++ show pCards)
        putStrLn ("Computer's hand:   " ++ show cCards)

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
--start blackjack newGame
