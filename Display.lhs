The Display module handles the majority of the work of displaying
menus and information for use in the main Solitaire module. Where
appropriate, it also handles getting user input from the command
line to be returned to the main module.

> module Display where

> import Card
> import Util
> import Control.Monad

The gameMenu function simply prints out the main game menu once
the user has begun playing and obtains the user input for that
game step, returning it to be used in the main module.

> gameMenu :: IO String
> gameMenu  = do putStrLn "\nSelect an option\n"
>                putStrLn "0: Draw"
>                putStrLn "1-7: Move card(s) from stack 1-7"
>                putStrLn "Q: Quit game\n"
>                o <- getLine
>                return o

The showAll function acts as a sort of wrapper function for
displaying the game board. It displays borders around a call
to showStacks, which actually displays the game board, followed
by displaying the elapsed game time and current score.

> showAll          :: [[Card]] -> Int -> Int -> Int -> IO ()
> showAll css n t p = do putStrLn "========================================="
>                        showStacks css n
>                        putStrLn "========================================="
>                        t2 <- getTime
>                        dispTime t t2
>                        dispScore t t2 p

The showStacks function displays the game board one entry at
a time. It does this by first calling the showStack function
for the first list argument passed to it and then recursively
calling showStacks for the remainder of the list of lists that
represents the game board.

> showStacks           :: [[Card]] -> Int -> IO ()
> showStacks []       _ = return ()
> showStacks (cs:css) n = do showStack cs n
>                            showStacks css (n + 1)

The showStack function displays one area, or stack, of the game
board. If the list passed to it contains a placeholder (i.e. the
stack is considered empty), then it displays blank lines. Otherwise,
it uses the vizStack and revVizStack functions to create a
visualization of the cards passed to it. It displays the number
of the stack before the cards in the stack as well, for ease of
use in determining the stack number while playing.

> showStack     :: [Card] -> Int -> IO ()
> showStack cs n = do putStr (show n ++ ". ")
>                     if (number $ head cs) == 0
>                      then putStr "\n\n\n"
>                      else do when (n < 10) $ putStr " "
>                              putStrLn $ (iterate (" --- "++) " --- ") !! ((length cs) - 1)
>                              putStr "    "
>                              vizStack $ reverse cs
>                              putStr "    "
>                              revVizStack $ reverse cs
>                              putStrLn ("    " ++ (iterate (" --- "++) " --- ") !! ((length cs) - 1))

The vizStack function displays the suit and number of the card
passed to it, followed by a recursive call to the rest of the
cards in the given list. If the card is not faceup, then a "*"
is displayed instead to represent the back of the card.

> vizStack       :: [Card] -> IO ()
> vizStack []     = putStr "\n"
> vizStack (c:cs) = do let s = if faceup c
>                               then [head $ charSuit (suit c)]
>                               else "*"
>                      let n = if faceup c
>                               then [head $ numToFace (number c)]
>                               else "*"
>                      putStr ("|" ++ n ++ " " ++ s ++ "|")
>                      vizStack cs

The revVizStack function is identical to vizStack, but it displays
the suit and number in reverse order.

> revVizStack       :: [Card] -> IO ()
> revVizStack []     = putStr "\n"
> revVizStack (c:cs) = do let s = if faceup c
>                                  then [head $ charSuit (suit c)]
>                                  else "*"
>                         let n = if faceup c
>                                  then [head $ numToFace (number c)]
>                                  else "*"
>                         putStr ("|" ++ s ++ " " ++ n ++ "|")
>                         revVizStack cs

The showDraw function takes a list of cards (the deck) and a number
and displays that number of cards from the top of the deck using
the showStack function. Before displaying, the flipUp function is
mapped to the cards to turn them faceup.

> showDraw     :: [Card] -> Int -> IO ()
> showDraw [] _ = putStrLn "No cards remaining in deck."
> showDraw cs n = showStack (map flipUp $ take n cs) 0

The dispTo function is called when the user would like to move a
card from one stack to another, returning the value obtained by
the call to getNum that represents the stack to move to.

> dispTo :: IO Int
> dispTo  = do putStr "Move to which stack? "
>              t <- getNum
>              return t

The dispNum function is called when moving multiple cards from
one stack to another and is identical to dispTo other than the
message printed.

> dispNum :: IO Int
> dispNum  = do putStr "Move how many? "
>               n <- getNum
>               return n

The getNum function gets a number as input from the user. The
atoi function is called on the number, and if it returns Nothing
(i.e. the input is not a number), an error message is printed
and getNum is called again.

> getNum :: IO Int
> getNum  = do s <- getLine
>              case atoi s of
>                Nothing -> putStr "Invalid option.\nEnter a number: " >> getNum
>                Just n  -> return n

The dispTime function takes the difference between the times passed
to it (the current time and the time at the start of the game) and
converts it to hours, minutes, and seconds, displaying it in the
format "hh:mm:ss".

> dispTime      :: Int -> Int -> IO ()
> dispTime t1 t2 = do let h = (((t2 - t1) `div` 3600) `mod` 100)
>                     let m = (((t2 - t1) `div` 60) `mod` 60)
>                     let s = ((t2 - t1) `mod` 60)
>                     putStr "\nGame time: "
>                     putStrLn $ (show' h ++ ":" ++ show' m ++ ":" ++ show' s)

The show' function is merely used to pad the show function for a
number with a leading 0 when the number is less than 10.

> show'        :: Int -> [Char]
> show' n
>   | n < 10    = "0" ++ show n
>   | otherwise = show n

The dispScore function calculates and displays the current game
score. The calculation for the score is p (current points based on
moves) + (5000 minus the current elapsed game time in seconds times 5).
The second part of this sum is taken as 0 if it would be negative,
using the maximum function.

> dispScore        :: Int -> Int -> Int -> IO ()
> dispScore t1 t2 p = putStrLn $ "Score: " ++
>                     (show (p + maximum [(5000 - ((t2 - t1) * 5)), 0])) ++ "\n"
