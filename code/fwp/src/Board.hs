module Board (
  Board (White, Red, RedKing, WhiteKing, Empty)
  ) where

data Board  = White | Red | RedKing | WhiteKing | Empty deriving (Eq,Show)
