This module contains miscellaneous functions for use with
the main module of Solitaire.

> module Util where 

> import Data.Char
> import Data.Time.Clock.POSIX

The atoi function takes in a String and returns the String
as an Int if it can be represented as such and Nothing
otherwise. This function comes from the CS457 examples
courtesy of Prof. Mark Jones.

> atoi   :: String -> Maybe Int
> atoi s  = if not (null s) && all isDigit s
>            then Just (read s)
>            else Nothing

The getSeed function is used to create a seed value for
random number generation. This is done by getting the
POSIX time, multiplying it by 1000000 to get the value
in microseconds, and passing the value to round to
transform it into an Int value.

> getSeed :: IO Int
> getSeed  = round . (*1000000) <$> getPOSIXTime

The getTime function is much like the getSeed function,
but it is used to retrieve the POSIX time in seconds
instead for use in keeping track of time passed.

> getTime :: IO Int
> getTime  = round <$> getPOSIXTime
