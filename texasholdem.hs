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

-- state consists of internal state and whether a player can Hit again or not
data State = State InternalState Bool Bool Bool String
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
blackjack act (State (pCards, cCards, deck, river, True) False cCanHit fiveCardsDrawn winningHand) =
    ContinueGame (State (pCards, cCards, deck, river, False) False cCanHit fiveCardsDrawn winningHand)
blackjack act (State (pCards, cCards, deck, river,  False) pCanHit False fiveCardsDrawn winningHand) =
    ContinueGame (State (pCards, cCards, deck, river, True) pCanHit False fiveCardsDrawn winningHand)

-- action = hit (needs to use checkSum to check the sum of a player's card values after a card was drawn,
--				 and drawFromDeck to draw a card from the deck)
--blackjack (Hit n) (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit) 
--    | currPlayer = checkSum (State (newCard:pCards, cCards, newDeck, river, not currPlayer) pCanHit cCanHit)
--    | otherwise = checkSum (State (pCards, newCard:cCards, newDeck, river, not currPlayer) pCanHit cCanHit)
--        where
--             (newCard,newDeck) = drawFromDeck deck n

-- action = stand (sets player's boolean flag (pCanHit/cCanHit) to False)
blackjack (Stand) (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand)
    | pCanHit == cCanHit && cCanHit == False = checkSum (State (pCards, cCards, deck, river, currPlayer) True True fiveCardsDrawn winningHand)
    | pCanHit == False || cCanHit == False = checkSum (State (pCards, cCards, deck, river, currPlayer) True True fiveCardsDrawn winningHand)
    | currPlayer = ContinueGame (State (pCards, cCards, deck, river, not currPlayer) False cCanHit fiveCardsDrawn winningHand)
    | otherwise = ContinueGame (State (pCards, cCards, deck, river, not currPlayer) pCanHit False fiveCardsDrawn winningHand)


drawForRiver :: State -> State
drawForRiver (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand) 
    | length river < 5 = (State (pCards, cCards, newDeckR, riverCard: river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand)
    | otherwise = (State (pCards, cCards, newDeckR, river, currPlayer) pCanHit cCanHit (not fiveCardsDrawn) winningHand)
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

-- 
processShuffledDeck :: IO [Card] -> [Card]
processShuffledDeck deck = unsafePerformIO deck

{-----------HELPER FUNCTIONS-----------}
-- State (pCards, cCards, deck, river, currPlayer)
startingDraw :: Int -> State -> State
startingDraw 0 (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand) = State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand
startingDraw n (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand) 
  | n > 5 = startingDraw (n-1) (State (newCard:pCards, cCards, newDeck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand)
  | n > 3 =  startingDraw (n-1) (State (pCards, newCard: cCards, newDeck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand)
  | otherwise = startingDraw (n-1) (State (pCards, cCards, newDeck, newCard:river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand)
    where 
        shuffleDeck = shuffle deck
        shuffledDeck = processShuffledDeck shuffleDeck 
        (newCard,newDeck) = drawFromDeck shuffledDeck 1

-- returns card drawn and new deck of cards (nth card is drawn from deck)


drawFromDeck :: [Card] -> Int -> (Card, [Card])
drawFromDeck deck n = (deck !! n, [card | card <- deck, card /= (deck !! n)])

-- adds up values of a list of cards
sumCards :: [Card] -> Int
sumCards [] = 0
sumCards ((_,val):tCard) = val + (sumCards tCard)

-- checks whether a player has exceeded 21
checkSum :: State -> Result
checkSum (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand)
    | pSum > 21 || (not pCanHit && not cCanHit && (21 - pSum) > (21 - cSum)) = EndOfGame False (State (pCards, cCards, deck, river, currPlayer) False False fiveCardsDrawn winningHand)
    | cSum > 21 || (not pCanHit && not cCanHit && (21 - pSum) < (21 - cSum)) = EndOfGame True (State (pCards, cCards, deck, river, currPlayer) False False fiveCardsDrawn winningHand)
    | (not pCanHit && not cCanHit && (21 - pSum) == (21 - cSum)) = Tie (State (pCards, cCards, deck,river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand)
    | otherwise = ContinueGame (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand)
      where
        pSum = sumCards pCards
        cSum = sumCards cCards

-- checks whether the ai or the player has the better hand
-- takes in current state of the game and returns a Result type 
checkWinner:: State -> Result 
checkWinner (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand)
    | compareHands pHand cHand == 1 = EndOfGame True (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn (fst pHand)) 
    | compareHands pHand cHand == 0 = EndOfGame False (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn (fst cHand))
    | otherwise = Tie (State (pCards, cCards, deck,river, currPlayer) pCanHit cCanHit fiveCardsDrawn "No winning Hand")
      where 
            pHandAndRiver = sortCardsAscending (concat[pCards, river])
            cHandAndRiver = sortCardsAscending (concat[cCards, river])
            pHand = checkHand pHandAndRiver pCards
            cHand = checkHand cHandAndRiver cCards


-- compares the hands of both the player and AI
-- returns an Int for the winner - 1 for the player, 0 for the AI, 2 for a tie 
compareHands:: ([Char], Int) -> ([Char], Int) -> Int
compareHands pHand cHand 
  | straightFlush /= 3 = straightFlush
  | fourOfAKind /= 3 = fourOfAKind
  | fullHouse /= 3 = fullHouse
  | flush /= 3 = flush
  | straight /=3 = straight
  | threeOfAKind /=3 = threeOfAKind
  | twoPair /= 3 = twoPair
  | pair /=3 = pair
  | otherwise = compareHandType "High Card" pHand cHand
   where straightFlush = compareHandType "Straight Flush" pHand cHand
         fourOfAKind = compareHandType "Four of a Kind" pHand cHand
         fullHouse = compareHandType "Full House" pHand cHand
         flush = compareHandType "Flush" pHand cHand
         straight = compareHandType "Straight" pHand cHand
         threeOfAKind = compareHandType "Three of a Kind" pHand cHand
         twoPair = compareHandType "Two Pair" pHand cHand
         pair = compareHandType "Pair" pHand cHand

   
        
-- compares a certain hand - passing in name of the hand 
-- returns 1 if player has that hand, 0 if AI has that hand, 2 if they tie, 3 if neither has the hand
compareHandType:: String -> ([Char], Int) -> ([Char], Int) -> Int
compareHandType handType pHand cHand
  | fst pHand == handType && fst cHand == handType = compareCardValues (snd pHand) (snd cHand)
  | fst pHand == handType = 1
  | fst cHand == handType = 0
  | otherwise = 3


-- compare card number values 
-- returns an Int for the higher card - 1 for the player, 0 for the AI, 2 for same value card
compareCardValues:: Int -> Int -> Int
compareCardValues pNumber cNumber 
  | pNumber > cNumber = 1
  | cNumber > pNumber = 0
  | otherwise = 2

-- checks what poker hand a player has
-- returns tuple with string, name of the hand, and the highest card number in the hand
checkHand:: [Card] -> [Card] -> (String, Int)
checkHand handWRiver hand
  | snd straightFlush /= 0 = straightFlush
  | snd fourOfAKind /= 0 = fourOfAKind
  | snd fullHouse /= 0 = fullHouse
  | snd flush /= 0 = flush
  | snd straight /= 0 = straight
  | snd threeOfAKind /= 0 = threeOfAKind
  | snd twoPair /= 0 = twoPair
  | snd pair /= 0 = pair
  | otherwise = checkHighCard hand 1
  where straightFlush = checkForStraightFlush handWRiver
        fourOfAKind = checkForFour handWRiver
        fullHouse = checkForFullhouse handWRiver
        flush = checkForFlush handWRiver
        straight = checkForStraight handWRiver
        threeOfAKind = checkForThree handWRiver 
        twoPair = checkForTwoPair handWRiver
        pair = checkForPair handWRiver 0



-- checks best hand and returns who wins
sortCardsAscending :: [Card] -> [Card]
sortCardsAscending hand = sortOn snd hand

checkHighCard :: [Card] -> Int -> ([Char], Int)         --Gets the highest value card from a list of cards
checkHighCard [] cardNum = ("High Card", cardNum)
checkHighCard (h:t) cardNum = if snd h > cardNum
                              then checkHighCard t (snd h)
                              else checkHighCard t cardNum

checkForPair :: [Card] -> Int -> ([Char], Int)      --Checks for a pair. If there are multiple pairs, higherPair determines whether the 
checkForPair hand higherPair = if higherPair == 0                          --higher pair or lower pair is returned
                         then case find (\x -> length x == 2) [filter (\c -> snd c == v) hand | v <- [1..13]] of
                               Just xs -> ("Pair", snd $ head xs)
                               Nothing -> ("Pair", 0)
                         else case find (\x -> length x == 2) [filter (\c -> snd c == v) hand | v <- reverse [1..13]] of
                               Just xs -> ("Pair", snd $ head xs)
                               Nothing -> ("Pair", 0)

checkForTwoPair :: [Card] -> ([Char], Int)
checkForTwoPair hand = if checkForPair hand 0 == checkForPair hand 1
                       then ("No Two Pair", 0)
                       else ("Two Pair", snd (checkForPair hand 0) + snd (checkForPair hand 1))

checkForThree :: [Card] -> ([Char], Int)
checkForThree hand = case find (\x -> length x == 3) [filter (\c -> snd c == v) hand | v <- [1..13]] of
                      Just xs -> ("Three of a Kind", snd $ head xs)
                      Nothing -> ("Three of a Kind", 0)
                   
checkForFour :: [Card] -> ([Char], Int)
checkForFour hand = case find (\x -> length x == 4) [filter (\c -> snd c == v) hand | v <- [1..13]] of
                      Just xs -> ("Four of a Kind", snd $ head xs)
                      Nothing -> ("Four of a Kind", 0)  


checkForStraight :: [Card] -> ([Char], Int)          --Need to sort by card value in ascending order using sortCardsAscending function before calling
checkForStraight (a:b:c:d:e:f:g:[]) = if [snd c, snd d, snd e, snd f, snd g] == [snd c, snd c+1, snd c+2, snd c+3, snd c+4]
                                      then ("Straight", snd g)
                                      else if [snd b, snd c, snd d, snd e, snd f] == [snd b, snd b+1, snd b+2, snd b+3, snd b+4]
                                           then ("Straight", snd f)
                                           else if [snd a, snd b, snd c, snd d, snd e] == [snd a, snd a+1, snd a+2, snd a+3, snd a+4]
                                                then ("Straight", snd e)
                                                else ("No straight", 0)
                    

checkForFlush :: [Card] -> ([Char], Int)    --checks for flush, better flush is the one with the highest card
checkForFlush [] = ("No Flush", 0)
checkForFlush hand = if length diamonds == 5
                        then ("Flush", snd (checkHighCard diamonds 0))
                        else if length hearts == 5
                             then ("Flush", snd (checkHighCard hearts 0))
                             else if length clubs == 5
                                  then ("Flush", snd (checkHighCard clubs 0))
                                  else if length spades == 5
                                       then ("Flush", snd (checkHighCard spades 0)) 
                                       else ("No Flush", 0)
                     where diamonds = filter (\c -> fst c == 'd') hand
                           hearts = filter (\c -> fst c == 'h') hand
                           clubs = filter (\c -> fst c == 'c') hand
                           spades = filter (\c -> fst c == 's') hand

checkForFullhouse :: [Card] -> ([Char], Int)
checkForFullhouse hand = if (snd (checkForThree hand) /= snd (checkForPair hand 0)) && snd (checkForThree hand) /= 0
                         then ("Full House", snd (checkForThree hand) + snd (checkForPair hand 0))
                         else if (snd (checkForThree hand) /= snd (checkForPair hand 1) && snd (checkForThree hand) /= 0)
                              then ("Full House", snd (checkForThree hand) + snd (checkForPair hand 0))
                              else ("No Full House", 0)


checkForStraightFlush :: [Card] -> ([Char], Int)        --Checks for a straight flush, parameter list needs to be sorted by card rank
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
aiDecide :: (Ord a, Num a, Num p) => [Card] -> a -> p
aiDecide cHand  pdecision 
   -- | (21 - fromIntegral (sumCards cHand)) < avg && ((sumCards pHand)-2) < (sumCards cHand)*4 `div` 3  = 1
   -- | (21 - fromIntegral (sumCards cHand)) < avg && ((sumCards pHand)-2) > (sumCards cHand) = 0
   -- | (21 - fromIntegral (sumCards cHand)) >= avg && (sumCards pHand) >= (sumCards cHand)*4 `div` 3 = 2
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
newGame = State ([], [], fullDeck, [], True) True True False ""

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
  let newState = startingDraw 7 state
      (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand) = newState in
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
          person_play game (ContinueGame (State (pCards, cCards, deck, river, True) pCanHit cCanHit fiveCardsDrawn winningHand)) (umoney, aimoney) 0
      else
          ai_play game (ContinueGame (State (pCards, cCards, deck, river, False) pCanHit cCanHit fiveCardsDrawn winningHand)) (umoney, aimoney) 0 0 0

-- Player IO handling function
--     0 == stand and don't bet
--     1 == stand and bet 
--     2 == draw and don't bet 
--     3 == draw and bet 
person_play :: Game -> Result -> Bet -> Int -> IO Bet
person_play game (ContinueGame state) (umoney, aimoney) value = 
    let (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand) = state in
      do
        putStrLn ("\nYour hand:         " ++ show pCards)
        putStrLn ("Computer's hand:   " ++ show cCards)
        putStrLn ("Current River      " ++ show river)
        putStrLn ("Your money:        " ++ show umoney)
        putStrLn ("Computer's money:  " ++ show aimoney)
        putStrLn ("Current pool:      " ++ show value)
        putStrLn ("Can you hit:       " ++ show pCanHit)
        putStrLn ("Can AI hit:        " ++ show cCanHit)
        num <- randomRIO (0, (length deck) - 1) :: IO Int

        if fiveCardsDrawn then
          do 
            putStrLn("howdy")
            person_play game (checkWinner state) (umoney, aimoney) value
        else 

        --  if pCanHit == False then
        --   ai_play game (game (Stand) state) (umoney, aimoney) value
          do
            --putStrLn ("How much do you want to bet?")
            putStrLn ("Do you want to check (1), bet(2) or fold(3)?")
            line <- validInput
            --num <- randomRIO (0, (length deck) - 1) :: IO Int
            if line == 1 then
              do
                -- CHECK
                putStrLn("You chose to check.")
                -- so user bets 0
                ai_play game (game (Stand) state) (umoney, aimoney) value 1 0--`debug` ( show $ state)
            else if line == 2 then 
              -- BET
              do
                  putStrLn ("How much would you like to bet?")
                  line <- moneyHandle umoney
                  if (line /= -1) then
                    let x = line :: Int in
                    ai_play game (game (Stand) (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand)) (umoney - x, aimoney) (x+value) 2 x--`debug` ( show $ state)
                  else
                    person_play game (Debt state) (umoney, aimoney) value
            else 
              -- FOLD
              person_play game (EndOfGame False state) (umoney, aimoney) value




person_play game (EndOfGame player state) (umoney, aimoney) value = 
    let (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand) = state in
    do
        putStrLn(winningHand)
        result <- update_bet state (umoney, aimoney) value player
        endOutput (state)
        play game newGame result

person_play game (Tie state) (umoney, aimoney) value = 
    do
       --putStrLn(winningHand)
       putStrLn ("Tie!")
       endOutput (state)
       play game newGame (umoney, aimoney)

person_play game (Debt state) (umoney, aimoney) value = 
    do
       putStrLn ("Bye! Until we meet again!")
       putStrLn ("-------------------------")
       start blackjack newGame

-- AI IO handling function
ai_play :: Game -> Result -> Bet -> Int -> Int -> Int -> IO Bet
ai_play game (ContinueGame state) (umoney, aimoney) value pDecision pBet =
    let riverState = drawForRiver state
        (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand) = riverState in
    do
      let aiDecision = aiDecide cCards pDecision
      print aiDecision
      let computerBet = aiBet cCards (avrg deck) aimoney aiDecision 
      num <- randomRIO (0, (length deck) - 1) :: IO Int
      if aiDecision == 1 -- CHECK
        then
            do
              let computerBet = 0
              putStrLn ("AI bet: " ++ show computerBet)
              putStrLn("--------------------------");
              person_play game (game (Stand) riverState) (umoney, aimoney) value
      else if aiDecision == 2 -- MATCH
        then
            do
              let computerBet = pBet
              putStrLn ("AI bet: " ++ show computerBet)
              putStrLn("--------------------------");
              
              person_play game (game (Stand) riverState) (umoney, aimoney - computerBet) (value + computerBet) 
      else -- FOLD
         do
              ai_play game (EndOfGame True riverState) (umoney, aimoney) value pDecision pBet
 
ai_play game (EndOfGame player state) (umoney, aimoney) value pDecision pBet=
    do
       result <- update_bet state (umoney, aimoney) value player
       endOutput (state)
       putStrLn("--------------------------");
       play game newGame result

ai_play game (Tie state) (umoney, aimoney) value pDecision pBet=
    do
       putStrLn ("Tie!")
       endOutput (state)
       putStrLn("--------------------------");
       play game newGame (umoney + (value `div` 2), aimoney + (value `div` 2))

-- updates the total bet pool and checks the state of the game
update_bet :: State -> Bet -> Int -> Bool -> IO Bet
update_bet (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand) (umoney, aimoney) value winner
   | winner == True = do 
      putStrLn("--------------------------");
      putStrLn ("You won!")
      x <- noMoney (umoney + value, aimoney);
      if(x > 1)
       then 
         start blackjack newGame
      else 
       return (umoney + value, aimoney)
   | winner == False  = do
      putStrLn("--------------------------");
      putStrLn ("AI won!")
      x <- noMoney (umoney, aimoney + value);
      if(x > 1)
       then 
         start blackjack newGame
      else 
       return (umoney, aimoney + value)
  

-- prints out hand at the end of a game
endOutput :: State -> IO ()
endOutput (State (pCards, cCards, deck, river, currPlayer) pCanHit cCanHit fiveCardsDrawn winningHand) = 
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
