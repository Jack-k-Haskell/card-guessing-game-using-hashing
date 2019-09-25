
module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List
import Data.Ord (comparing)

-- hardcoding allcards to save time could reduce edit this
allCards = [(Card Club R2) .. (Card Spade Ace)]
allCards2 = combinations 2 allCards
allCards3 = combinations 3 allCards
allCards4 = combinations 4 allCards
allCards5 = combinations 4 allCards

-- hardcoding the guesses
firstGuess2 = [(Card Spade R5), (Card Club R7)]
firstGuess3 = [(Card Spade R4), (Card Club R7), (Card Heart R10)]
firstGuess4 = [(Card Spade R4), (Card Club R7), (Card Heart R10), (Card Diamond King)]
firstGuess5 = [(Card Spade R4), (Card Club R7), (Card Heart R10), (Card Diamond King), (Card Heart R3)]

-- a combination of cards for gamestate
data GameState = GameState { possibilities :: [[Card]]} deriving (Show)

-- functions to insert into higherorder compare functions
-- ( i dont know how i got away without declearing this lol )
isSameSuit (Card s r) (Card s2 r2)= s == s2
isSameRank (Card s r) (Card s2 r2)= r == r2
isLowerRank (Card s r) (Card s2 r2)= r > r2
isHigherRank (Card s r) (Card s2 r2)= r < r2
isSameCard c c2= c == c2


-- checks how many similar cards in in 2 list when proved a compare function
compareSames :: (Card -> Card -> Bool) -> [Card] -> [Card] -> Int
compareSames _ [] _ = 0
compareSames func a b = length (nub [ x | x <-a, y<-b, func x y])

-- small sorting by rank function -- doesnt work for singular
sortByRank :: [Card] -> [Card]
sortByRank = sortBy (comparing rank)

{-
 take the lowest card , then filter out cards that arent lower then the
 lowest number in the first input and lengths them
-}
compareLowers :: [Card] -> [Card] -> Int
compareLowers (x:xs) [] = 0
compareLowers ((Card s r):xs) ( y : ys) = length lessthanlist
    where lessthanlist = filter (( r> ) . rank) ( y: ys)

{-
 take the highest card , then filter out cards that arent lower then the
 lowest number in the first input and lengths them
 TODO
 ill combine those functions later
-}
compareHighers :: [Card] -> [Card] -> Int
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
-- hardcoded to save time
initialGuess :: Int -> ([Card],GameState)
initialGuess n
  | n == 2 = (firstGuess2 , GameState allCards2)
  | n == 3 = (firstGuess3 , GameState allCards3)
  | n == 4 = (firstGuess4 , GameState allCards4)
  | n == 5 = (firstGuess5 , GameState allCards5)
  | otherwise = error "Number of cards required to guess not in  [2,3,4,5] "

--------------------------
{-
  1. feedback produces a semi-unique output for each awns, guess pair
  2. using feedback x guess, we can produce a semi-unique hash from the possiblitie pool
  3. repeat
  4. profit?
-}
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (cards, gs) ( noCorrect, noLower, noSame, noHigher, noSuit)
  = (nextGuess, (GameState filteredPoss) )
    where
        n = length cards
        filteredPoss = filterGuesses fb cards (possibilities gs) -- feeds possibilities in feedback and returns the ones that match the feedback signature
        nextGuess = nextBestGuess filteredPoss
        fb = ( noCorrect, noLower, noSame, noHigher, noSuit)


-- efficient algorithm to produce combonations
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

-- tests everything in the list of possible combonations through feedback to see which produce the same signature
-- essentially a hashing algorithm
filterGuesses :: (Int,Int,Int,Int,Int) -> [Card] ->[[Card]] -> [[Card]]
filterGuesses fb guess oldposs =
    [x | x<- oldposs, feedback x guess == fb]


{-
  takes a list of all possiblities and returns the middle one
-}
nextBestGuess:: [[Card]] -> [Card]
nextBestGuess [] = error "impossible matching"
nextBestGuess (poss:xs)
  | longness == 0 = error "reached impossible state, there are no cards in poss that match this feedback signature"
  | longness == 1 = poss
  | otherwise = (poss:xs) !! (longness `div` 2)
    where
        longness = length (poss:xs)

----------for debugging
main =
  putStrLn "main started sucessfully"
