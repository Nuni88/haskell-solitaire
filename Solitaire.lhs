The Main module for Solitaire. This module handles the main game logic,
calling functions from the Card, Util, and Display modules as needed.
The Solitaire game implemented here contains modes for draw 1 and draw 3,
as well as tracking time played and a score value based on the time
taken and the moves made by the player.

> module Main(main) where

> import Card
> import Util
> import Display
> import System.Random
> import Data.Char

The main function is the entry point for the Solitaire program.
Here, a random seed is initialized with getSeed, a menu is presented
to the user to choose a game mode, and a game is started based on
the user's choice, passing in a shuffled and dealt deck of cards to
the play function.

> main :: IO ()
> main  = do g <- getSeed
>            putStrLn "\nChoose an option from the menu below:\n"
>            putStrLn "1. Draw One"
>            putStrLn "2. Draw Three"
>            putStrLn "3. Exit\n"
>            o <- getLine
>            case atoi o of
>              Just n  -> case n of
>                           1         -> play (deal $ shuffle deck 51 (mkStdGen g)) 1
>                           2         -> play (deal $ shuffle deck 51 (mkStdGen g)) 3
>                           3         -> putStrLn "Thanks for playing!"
>                           otherwise -> do putStrLn "Invalid option."
>                                           main
>              Nothing -> do putStrLn "Invalid option."
>                            main

The play function handles functionality required before and after
the actual start of the game. This includes recording the time at
the start of the game and displaying the results of a successful
game (including final time and score). Finally, the function
completes by calling main once again.

> play      :: [[Card]] -> Int -> IO ()
> play css n = do t1 <- getTime
>                 (win, p) <- game css n 5000 t1
>                 if win
>                  then do t2 <- getTime
>                          putStrLn "\nCongratulations, you win!"
>                          dispTime t1 t2
>                          dispScore t1 t2 p
>                          main
>                  else main

The game function handles checking the win condition for the game
and getting the user's option at each game step via the gameMenu
function. Once the option has been processed to ensure that it is
valid, the game function passes the input to the doOption function
to parse the option.

> game          :: [[Card]] -> Int -> Int -> Int -> IO (Bool, Int)
> game css n p t = do if checkWin css
>                      then return (True, p)
>                      else do showAll (tail css) 1 t p
>                              o <- gameMenu
>                              if (toUpper $ head o) == 'Q'
>                               then return (False, p)
>                               else case atoi o of
>                                      Just x  -> do (css', p') <- doOption css x n p t
>                                                    game css' n p' t
>                                      Nothing -> putStrLn "Invalid option." >> game css n p t

The doOption function parses the user input from the game function
to determine which course of action to take. If the option is 0,
then the user has chosen to draw from the deck, so the showDraw
and playDraw functions are invoked, unless there are no cards
remaining in the deck, in which case a message is printed to indicate
this.

If the option is between 1 and 7, then the user is moving
cards from the field of that number to another. The algorithm
uses checks to determine whether moving multiple cards is possible
and prompts the user for a number of cards to move only when
necessary, calling the moveMore function. Otherwise, it calls the
moveOne function.

If the option is not between 0 and 7, an error message is printed
instead. In any case, the function returns a tuple with the new
game state and the new score.

> doOption                :: [[Card]] -> Int -> Int -> Int -> Int -> IO ([[Card]], Int)
> doOption css f n p t
>   | f == 0               = if number (head $ head css) == 0
>                             then putStrLn "No cards left in deck." >> return (css, p)
>                             else do putStrLn $ "Cards left in deck: " ++ (show $ length $ head css)
>                                     let n' = minimum [(length $ head css), n]
>                                     showDraw (head css) n'
>                                     (css', p') <- playDraw css n' p t
>                                     return (css', p')
>   | (f >= 1) && (f <= 7) = if (length (css !! f)) > 1
>                             then do t <- dispTo
>                                     if t < 8 && faceup ((css !! f) !! 1)
>                                      then do n <- dispNum
>                                              (css', p') <- moveMore css f t n p
>                                              return (css', p')
>                                      else do (css', p') <- moveOne css f t p
>                                              return (css', p')
>                             else if number (head $ css !! f) /= 0
>                                   then do t <- dispTo
>                                           (css', p') <- moveOne css f t p
>                                           return (css', p')
>                                   else putStrLn "No cards to move." >> return (css, p)
>   | otherwise            = putStrLn "Invalid option." >> return (css, p)

The playDraw function handles playing cards from the deck
to other game fields. The actions it takes are based on the
range in which a number entered exists:

If the player enters 0, then no cards are played and the
original game state is returned with a reduced score.

If the player enters 1-7, then checkPlay is called to determine
if the play is valid, with moveCard being called if so to
change the game state appropriately. The new game state and
the original score with 25 points added are returned.

If the player enters 8-11, then checkOther is called for
playing a card to its final position. If valid, then the
game state is changed appropriately and 100 points are added
to the player's score.

Any other number will result in an error. Finally, if the
game mode is draw three, then playDraw will be called recursively
if a card is played until no cards in the original draw of
three are left, the user enters 0, or no cards are left in
the deck.

> playDraw          :: [[Card]] -> Int -> Int -> Int -> IO ([[Card]], Int)
> playDraw css n p t = do putStrLn ((numToFace (number $ head $ take n $ head css))
>                          ++ " of " ++ (charSuit (suit $ head $ take n $ head css)))
>                         putStr "Play to which stack (0 to pass)? "
>                         o <- getLine
>                         case atoi o of
>                           Nothing -> putStrLn "Invalid option." >> playDraw css n p t
>                           Just x  -> if x > 11
>                                       then putStrLn "Invalid option." >> playDraw css n p t
>                                       else if x <= 0
>                                             then return (([(drop n $ head css) ++ (take n $ head css)] ++ tail css), (p - (10 * n)))
>                                             else if f (head $ css !! x) (head $ head css)
>                                                   then if n > 1
>                                                         then do let css' = moveCard css 0 x
>                                                                 showAll (tail css') 1 t p
>                                                                 showDraw (head css') (n - 1)
>                                                                 playDraw css' (n - 1) p' t
>                                                         else return ((moveCard css 0 x), p')
>                                                   else putStrLn "Invalid move." >> playDraw css n p t
>                                                   where
>                                                     (f, p') = if x > 7
>                                                                then (checkOther, (p + 100))
>                                                                else (checkPlay, (p + 25))

The moveOne function moves one card from the game field f (from)
to the game field t (to). The value for f must be between 1 and 7,
t must be between 1 and 11, and f must not equal t. Violations of
any of these requirements will result in an error message with
the original game state and score returned. Otherwise, movement
is handled much like in playDraw, with the checking based on the
value of t. Moves to the final position add 100 to the score, while
other moves reduce the score by 10, rewarding lower numbers of
total moves.

> moveOne      :: [[Card]] -> Int -> Int -> Int -> IO ([[Card]], Int)
> moveOne css f t p
>   | or [(f < 1),(f > 7),(t < 1),(t > 11),(f == t)]
>               = putStrLn "Invalid move." >> return (css, p)
>   | otherwise = if fun (head $ css !! t) (head $ css !! f)
>                  then return ((moveCard css f t), p')
>                  else putStrLn "Invalid move." >> return (css, p)
>                  where
>                    (fun, p') = if t > 7
>                                 then (checkOther, (p + 100))
>                                 else (checkPlay, (p - 10))

The moveMore function is similar to moveOne, but for moving multiple
cards from a stack at once. The number of cards to be moved must
not be more than the number of cards in stack f, nor can it be 0.
If the conditions are met, checkPlay is used to determine if the
move is valid, and moveCards is then called for valid moves.

> moveMore     :: [[Card]] -> Int -> Int -> Int -> Int -> IO ([[Card]], Int)
> moveMore css f t n p
>   | or [(f < 1),(f > 7),(t < 1),(t > 7),(f == t),(n > (length (css !! f))),(n == 0)]
>               = putStrLn "Invalid move." >> return (css, p)
>   | otherwise = if checkPlay (head $ css !! t) (head $ reverse $ take n $ css !! f)
>                  then if faceup $ head $ reverse $ take n $ css !! f
>                        then return ((moveCards css f t n), (p - 10))
>                        else putStrLn "Invalid move." >> return (css, p)
>                  else putStrLn "Invalid move." >> return (css, p)

The checkPlay function returns True if the number of the card
to be played to is one greater than the card to be played and
the colors of the cards are different, returning False otherwise.
The function also returns True if the card to be played on top
of is the placeholder, ph, as this represents an empty field.

> checkPlay           :: Card -> Card -> Bool
> checkPlay c1 c2
>   | (number c1) == 0 = True
>   | otherwise        = if (color c1) /= (color c2)
>                         then if (number c1 == ((number c2) + 1))
>                               then True
>                               else False
>                         else False

The checkOther function is for moving cards to their final
positions, so it returns True if the card to be played has a
number 1 greater than the card to be played to and if they are
the same suit OR if the card to be played to is ph and the card
to be played is an Ace, returning False otherwise.

> checkOther          :: Card -> Card -> Bool
> checkOther c1 c2
>   | (number c1) == 0 = (number c2) == 1
>   | otherwise        = if (suit c1) == (suit c2)
>                         then if (number c1 == (number c2) - 1)
>                               then True
>                               else False
>                         else False

The checkWin function simply checks for the win condition of
the game using the and function across a list comprehension
of the last four game fields (the final positions) to see if
they each contain 13 cards (the 13 cards for their respective
suits).

> checkWin    :: [[Card]] -> Bool
> checkWin css = and [length (css !! n) == 13 | n <- [8..11]]
