
--Implement your solution here
--Remember to put function declarations as well
--module Proj1 (feedback, initialGuess, nextGuess, GameState) where
--module Proj1 (feedback) where

-- before modifying gamestate to be list of lists of cards
import Card
import Data.List
import Data.Ord (comparing)
{-
data PossRank =
    Rank | Not Rank | NoIdea
    deriving (Eq,Show)

data PossSuit =
    Suit | Not Suit | NoIdea
    deriving (Eq,Show)
-}
-- hardcoding allcards to save time could reduce edit this
allCards = [(Card Club R2) .. (Card Spade Ace)]
allCards2 = combinations 2 allCards
allCards3 = combinations 3 allCards
allCards4 = combinations 4 allCards

noSuits = 4
noCards = length allCards
noCardsPerSuit = noCards `div` noSuits

data GameState = GameState {
            {-
             - needs to have
             - n list of card possibilities or just 1
             - exact matches
             - previous feedbacks, guess in a list of tuples
            -}
                       possibilities :: [[Card]]
                      --, lastGuesses :: [[Card]]
                     } deriving (Show)

-- functions to insert into higherorder functions
-- ( i dont know how i got away without declearing this )
isSameSuit (Card s r) (Card s2 r2)= s == s2
isSameRank (Card s r) (Card s2 r2)= r == r2
isLowerRank (Card s r) (Card s2 r2)= r > r2
isHigherRank (Card s r) (Card s2 r2)= r < r2
isSameCard c c2= c == c2


-- checks how many similar cards in in 2 list
compareSames :: (Card -> Card -> Bool) -> [Card] -> [Card] -> Int
compareSames _ [] _ = 0
{-
compareSames func a b =
    if func (head a) (head b)
        1 + compareSames func (tail a) (tail b)
    else
        compareSames func (tail a) (tail b)
-}
compareSames func a b = length [ x | x <-a, y<-b, func x y]
-- small sorting by rank function -- doesnt work for singular
sortByRank :: [Card] -> [Card]
sortByRank = sortBy (comparing rank)

compareLowers :: [Card] -> [Card] -> Int
{-
 take the lowest card , then filter out cards that arent lower then the
 lowest number in the first input and lengths them
-}
compareLowers (x:xs) [] = 0
compareLowers ((Card s r):xs) ( y : ys) = length lessthanlist
    where lessthanlist = filter (( r> ) . rank) ( y: ys)

compareHighers :: [Card] -> [Card] -> Int
{-
 take the highest card , then filter out cards that arent lower then the
 lowest number in the first input and lengths them
 TODO
 ill combine those functions later

-}
compareHighers (x:xs) [] = 0
compareHighers x ( y : ys) = length lessthanlist
    where
        lessthanlist = filter (( r< ) . rank) ( y: ys)
        ((Card s r):xs) = reverse x


feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback awns guess  = ( noCorrect, noLower, noSame, noHigher, noSuit)
  where
    noCorrect = compareSames isSameCard awns guess
    noLower = compareLowers rankedGuess rankedAwns
    noSame = compareSames isSameRank awns guess
    noHigher = compareHighers rankedGuess rankedAwns
    noSuit = compareSames isSameSuit awns guess
    rankedAwns = sortByRank awns
    rankedGuess = sortByRank guess

----------------------

initialGuess :: Int -> ([Card],GameState)
{-
For the first guess, when the list of possible answers is at its longest, you may want to
use a different approach. Given the way the feedback works, the best first guess would
be to choose two cards of different suits and with ranks about equally distant from each
other and from the top and bottom ranks. In general, for an n card answer, you should
choose ranks that are about 13/(n + 1) ranks apart.

todo
- cards
    the number of cards in 13/(2 + 1), so that would 6 which would select the
    6th element selected from the list, while the next selection would be 6 + 13 from the possible list
- initate possibilities in gamestate

-}

initialGuess n = ( guessList , GameState allCards  )
    where
        guessList = cardSelect distanceBetweenCards n noCardsPerSuit 0 allCards
        distanceBetweenCards = noCardsPerSuit `div` (n + 1)


cardSelect :: Int -> Int -> Int -> Int ->  [Card] -> [Card]
{-
    - takes number of cards to be selected and current number
    - just put the increment in the first place and increments itsself + suit and returns it

    - edit it so it its doesnt take cnstants such as all suits or noCardsPerSuit
    - could prob make it n times
    oritinally it was distance = distance *2 +skip
-}
cardSelect _ 0 _ _ _ = []
cardSelect distance noGuesses skip startAt poss  =
    (cardSelect distance (noGuesses-1) (skip) (startAt+skip) poss)  ++ [poss !! (startAt + (distance *noGuesses))]

--------------------------
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
{-
    takes as input a pair of the previous guess and game state, and the feedback to this
    guess as a quintuple of counts of correct cards, low ranks, correct ranks, high ranks,
    and correct suits, and returns a pair of the next guess and new game state.

    - implement rulesets to filter out the 0s and the ns check

    - also make it ask the user for input out when its stuck
    - currently not returning a new guess out of the possibilities
    - add noHigher and noLower = 0 at the same time
    - add noHigher adn nolower = n then poss \\ ( filter islowerrank than the highest and ishigherank than the lowest )

    - needs to modify the filter function to accept list of lists
    - needs to make gamestate a card [][]

    for test
    nextGuess ([c3,s5], GameState allCards) ( 0, 0, 0, 2, 0)
-}
nextGuess (cards, gs) ( noCorrect, noLower, noSame, noHigher, noSuit)
  =
    where
        n = length cards
        poss = sortByRank (possibilities gs) -- [Card] potentially dont need to sort
        lg = sortByRank (cards) -- sort by rank doesnt work for singlar input, otherwise its fine
        filteredPoss = filterguesses fb cards (possibilities gs) -- feeds possibilities in feedback and returns the ones that match the feedback signature
        nextGuess = nextBestGuess n filteredPoss
        fb = ( noCorrect, noLower, noSame, noHigher, noSuit)
        -- lgs = lastGuesses gs -- [[Card]]
        -- lg = sortByRank (head lgs)



filterer ::  (Card -> Card -> Bool) -> [Card] -> [Card] -> [Card]
-- similar to compareSame, but produces a list instead of int
filterer func lg poss = [ y  | x <-lg , y <-poss, func x y ] -- something about an order
{-
 - generate a poss 2, 3, 4 which is a cardlist of avalible combonations


-}

combinations :: Int -> [a] -> [[a]] -- the fastest sort
combinations 0 _ = [[]]
combinations _ [] = []
combinations n xs@(y:ys)
  | n < 0     = []
  | otherwise = case drop (n-1) xs of
                [ ] -> []
                [_] -> [xs]
                _   -> [y:c | c <- combinations (n-1) ys]
                          ++ combinations n ys

{-
  tests everything in the list of possible combonations to
-}

filterguesses :: (Int,Int,Int,Int,Int) -> [Card] ->[[Card]] -> [[Card]]
filterguesses fb guess oldposs =
    [x | x<- oldposs, feedback x guess == fb]


{-
  it recieves a filtered list and length `div` (n+1)
  -- could probebly delete this function
-}
nextBestGuess:: Int -> [Card] -> [Card]
nextBestGuess n poss
  | n == length poss = poss
  | otherwise = cardSelect distance n 0 0 poss
    where distance = (length poss) `div` (n+1)


--
-- combos 1 list = map (\x -{> [x]) list
-- combos n list = foldl (++) [] $ map (\x -> map (\y -> x:y) nxt) list
--     where nxt = combos (n-1) list}
-- rmdups ( combos n allCards) -- where n is length

-- rmdups :: (Ord a) => [a] -> [a]
-- rmdups = map head . group . sort
-- testing ------------------
c2 = Card Club R2
c3 = Card Club R3
s4 = Card Spade R4
s5 = Card Spade R5

lista = [ c3, s4]
listb = [ c2, s5]
listg = [
    (Card Spade R5),(Card Spade R3),(Card Spade R7),(Card Spade R10),
    (Card Diamond Ace),(Card Diamond R10),(Card Diamond R4),(Card Diamond R5),
    (Card Heart Jack),(Card Heart Ace),(Card Heart R5),(Card Heart R6)
        ]
listp = [ allCards !!26,allCards !! 8 ]


----------for debugging


main =
  putStrLn "main started sucessfully"



{-
takes a target and a guess (in that order), each represented as a list of Cards, and
returns the five feedback numbers, as explained above, as a tuple

1. How many of the cards in the answer are also in the guess (correct cards).
 - match strings

2. How many cards in the answer have rank lower than the lowest rank in the guess (lower
ranks). Ranks, in order from low to high, are 2–10, Jack, Queen, King, and Ace.
 - pick the lowest rank from list 2 and see how many are lower than that

3. How many of the cards in the answer have the same rank as a card in the guess (correct
ranks). For this, each card in the guess is only counted once. That is, if the answer has
two queens and the guess has one, the correct ranks number would be 1, not 2. Likewise
if there is one queen in the answer and two in the guess.
- list comprehension , if a card is correct, ++1 and skip to the next one

4. How many cards in the answer have rank higher than the highest rank in the guess
(higher ranks).
5. How many of the cards in the answer have the same suit as a card in the guess, only
counting a card in the guess once (correct suits). For example, if the answer has two
clubs and the guess has one club, or vice versa, the correct suits number would be 1,
not 2.

-}
