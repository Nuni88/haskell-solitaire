The Card module contains the majority of the code for representing
and manipulating cards.

> module Card where

> import Data.List
> import System.Random

The Card data type contains fields for the number, suit, and color
of a card and for whether or not the card is faceup. Number, suit,
and color are represented by Integers to simplify operations that
involve them.

> data Card = MkCard
>   { faceup :: Bool
>   , number :: Integer     -- Values between 1 and 13
>   , suit   :: Integer     -- Values between 1 and 4
>   , color  :: Integer     -- Values between 0 and 1
>   } deriving (Show, Eq)

The Card variable, ph, is a placeholder card with all 0 values.
This card is used in manipulating the game fields by ensuring that
the list containing all game fields will not change in length when
a field is "empty" by copying ph into that field.

> ph = MkCard { faceup = False, number = 0, suit = 0, color = 0 }

The deck function builds the deck of cards (a list of cards) with the
Card data constructor. It uses list comprehensions to build cards
with numbers from 1-13 for suits 1 and 2 with color 0 and for suits
3 and 4 with color 1.

> deck :: [Card]
> deck  = [MkCard { faceup = False, number = n, suit = s, color = 0 }
>          | s <- [1,2], n <- [1..13]] ++
>         [MkCard { faceup = False, number = n, suit = s, color = 1 }
>          | s <- [3,4], n <- [1..13]]

The deal function takes a list of cards and transforms it into a list
of lists, with the head representing the deck, the next 7 indices
representing the main Solitaire playing fields, and adding ph to
four more indices for the final areas for each suit. Each list is
created with n cards from the top of the deck, from 7 to 1, with
the head of each list set to be faceup by mapping the flipEnd
function.

> deal   :: [Card] -> [[Card]]
> deal cs = [drop (sum [1..7]) cs] ++
>           (map flipEnd [take m (drop (sum [m..7] - m) cs) | m <- (reverse [1..7])]) ++
>           [[ph] | i <- [1..4]]

The shuffle function takes a list, or deck, of cards, removes a random
card from it using the randomR function, adds the card to a new list,
and is then called recursively for the remainder of the original deck,
creating a list with all the original cards in a randomized ordering.

> shuffle      :: [Card] -> Int -> StdGen -> [Card]
> shuffle cs n g
>   | n < 0     = []
>   | otherwise = [cs !! (fst $ randomR (0, n) g)] ++
>                 shuffle (delete (cs !! (fst $ randomR (0, n) g)) cs) (n - 1) (snd $ randomR (0, n) g)

The flipEnd function takes a list of cards and changes the head to
be faceup.

> flipEnd   :: [Card] -> [Card]
> flipEnd cs = [(head cs) { faceup = True }] ++ tail cs

The flipUp function takes a card and turns it faceup.

> flipUp  :: Card -> Card
> flipUp c = c { faceup = True }

The moveCard function is used to move a single card from one game
field to another. This is done by splitting the list of game fields
into three pieces, with splits at the two fields to be modified. The
value n is the from field and m is the to field, but as it is unknown
which value is smaller, the algorithm determines where each split
occurs and which cards are moved by comparing m and n. Ultimately,
the list of game fields will be modified so that the head of the from
field is moved to the head of the to field, and if the from field
would be left empty, then ph is placed into that field to retain the
proper indexing. Additionally, if cards would be left in the from
field, then the new head of that field is set to be faceup if it
was not previously.

> moveCard        :: [[Card]] -> Int -> Int -> [[Card]]
> moveCard css n m = css'
>   where
>     c           = (head $ css !! n) { faceup = True }
>     f           = minimum [m,n]
>     s           = (maximum [m,n]) - f
>     (xss, yss)  = splitAt f css
>     (yss', zss) = splitAt s yss
>     css'        = xss ++ s1' ++ tail yss' ++ s2' ++ tail zss
>                   where
>                     s1  = if length (head css1) > 1
>                            then if faceup $ head $ tail $ head css1
>                                  then [tail $ head css1]
>                                  else [flipEnd $ tail $ head css1]
>                            else [[ph]]
>                           where
>                             css1 = if f == n
>                                     then yss'
>                                     else zss
>                     s2  = if number (head $ head css2) == 0
>                            then [[c]]
>                            else [[c] ++ (head css2)]
>                           where
>                             css2 = if f == n
>                                     then zss
>                                     else yss'
>                     s1' = if f == n
>                            then s1
>                            else s2
>                     s2' = if f == n
>                            then s2
>                            else s1

The moveCards function works very similarly to the moveCard function,
but it handles moving multiple cards at once rather than a single card.

> moveCards          :: [[Card]] -> Int -> Int -> Int -> [[Card]]
> moveCards css n m x = css'
>   where
>     cs          = take x $ css !! n
>     f           = minimum [m,n]
>     s           = (maximum [m,n]) - f
>     (xss, yss)  = splitAt f css
>     (yss', zss) = splitAt s yss
>     css'        = xss ++ s1' ++ tail yss' ++ s2' ++ tail zss
>                   where
>                     s1  = if length (drop x (head css1)) >= 1
>                            then if faceup $ head $ drop x (head css1)
>                                  then [drop x (head css1)]
>                                  else [flipEnd $ drop x (head css1)]
>                            else [[ph]]
>                           where
>                             css1 = if f == n
>                                     then yss'
>                                     else zss
>                     s2  = if number (head $ head css2) == 0
>                            then [cs]
>                            else [cs ++ (head css2)]
>                           where
>                             css2 = if f == n
>                                     then zss
>                                     else yss'
>                     s1' = if f == n
>                            then s1
>                            else s2
>                     s2' = if f == n
>                            then s2
>                            else s1

The charSuit function simply returns a String interpretation of the
suit value of a card.

> charSuit  :: Integer -> [Char]
> charSuit n = case n of
>                1         -> "Clubs"
>                2         -> "Spades"
>                3         -> "Diamonds"
>                4         -> "Hearts"
>                otherwise -> "?"

The charColor function returns a Char interpretation of the color of
a card.

> charColor  :: Integer -> Char
> charColor n = case n of
>                 0         -> 'B'
>                 1         -> 'R'
>                 otherwise -> '?'

The numToFace function returns a String interpretation of the number
of a face card. Ten is included here to aid in formatting in
displaying the game board (as each number must be one digit).

> numToFace  :: Integer -> [Char]
> numToFace n = case n of
>                1         -> "Ace"
>                10        -> "Ten"
>                11        -> "Jack"
>                12        -> "Queen"
>                13        -> "King"
>                otherwise -> (show n)
