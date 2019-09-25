# ai-card-guessing-game-using-hashing
95%~ efficiency up to 5 cards guesses.

A card guessing game which involves 2 parties
The guesser and the master.
The game is played with a guesser deck and a master deck, meaning no duplicate cards in either the guess or the responce by the master.

The master picks out 1~5 cards from the deck (no duplicates) and gives the guesser ai the number of cards to guess.
Then the guesser returns the starting guess (hard coded).


Then the master returns with the following information. ( if a answer card matches the guess card it is consumed and cannot match again)
1. How many of the cards in the answer are also in the guess (correct cards).
2. How many cards in the answer have rank lower than the lowest rank in the guess (lower ranks). Ranks, in order from low to high, are 2–10, Jack, Queen, King, and Ace.
3. How many of the cards in the answer have the same rank as a card in the guess (correct ranks). For this, each card in the guess is only counted once. That is, if the answer has two queens and the guess has one, the correct ranks number would be 1, not 2. Likewise if there is one queen in the answer and two in the guess.
4.How many cards in the answer have rank higher than the highest rank in the guess (higher ranks).
5.How many of the cards in the answer have the same suit as a card in the guess, only counting a card in the guess once (correct suits). For example, if the answer has two clubs and the guess has one club, or vice versa, the correct suits number would be 1, not 2.
