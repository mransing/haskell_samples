{- A simple progmac game. The player can be on any point on 2x2 grid, facing any one the four direction. A player can rotate eithr to left or to right, and can take a step of one point.
-}

maxlimit = 10 

data Direction = Up| RRight| Down| LLeft deriving Show
data Point = Point {x :: Integer, y:: Integer} deriving Show
data Position = Position {pp::Point, pd::Direction} | OutOfBound deriving Show

goleft :: Position -> Position 
goleft OutOfBound = OutOfBound
goleft (Position p Up) = Position p LLeft
goleft (Position p LLeft) = Position p Down 
goleft (Position p Down) = Position p RRight 
goleft (Position p RRight) = Position p Up

goright :: Position -> Position
goright OutOfBound = OutOfBound
goright (Position p Up) = Position p RRight
goright (Position p LLeft) = Position p Up 
goright (Position p Down) = Position p LLeft 
goright (Position p RRight) = Position p Down

gostr :: Position -> Position
gostr OutOfBound = OutOfBound
gostr (Position (Point x y) Up ) = if (y + 1) == maxlimit then OutOfBound else Position (Point x (y+1) ) Up
gostr (Position (Point x y) LLeft) = if (x - 1) == -maxlimit then OutOfBound else Position (Point (x-1) y) LLeft
gostr (Position (Point x y) Down) = if (y - 1) == -maxlimit then OutOfBound else Position (Point x (y-1)) Down
gostr (Position (Point x y) RRight) = if (x + 1) == maxlimit then OutOfBound else Position (Point (x+1) y) RRight

run :: Position -> (Position -> Position) -> Position
d `run` f = f d

main = do
     y <- return (Position (Point 0 0) Up)
     z <- return (goleft y)
     a <- return (gostr z)
     print a
     b <- return (Position (Point 9 1) RRight)
     c <- return (gostr b)
     print (b, c) 
     print (Point 5 5, gostr $ goleft $ gostr $ goright $ Position (Point 5 5) Up, (Position (Point 0 0) Up) `run` goleft `run` gostr `run` goright `run` gostr `run` goleft `run` goleft `run` gostr `run` gostr)
       
