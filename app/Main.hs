module Main where

import OperationalTransformation

main :: IO ()
main = do
  print ( "ca", ca )
  print ( "cb", cb )
  print ( "moves", moves )
  print ( "t0", t0 )
  print ( "f0", f0 )
  print ( "f1", f1 )
  print ( "f2", f2 )
  print ( "final", final)

data Move = Move Int | Stay deriving Show

data Thing = Thing Int deriving Show

instance Operation Move where
  transform a b = (b, a)


move :: Move -> Thing -> Thing
move m (Thing pos) = Thing $ dist m + pos
  where dist (Move x) = x
        dist Stay = 0

as :: [Move]
as = [Move 3,    Move (-4), Move 7,     Stay, Stay,     Move (-6)]

bs :: [Move]
bs = [Move (-3), Stay,      Move (-17), Stay, Move 100, Move (-80)]

combine :: Move -> Move -> Move
combine  a b = Move $ dist a + dist b
  where dist (Move x) = x
        dist Stay = 0


ca = foldl combine Stay as
cb = foldl combine Stay bs

moves = zip as bs

frame :: Thing -> ( Move, Move ) ->  (Move, Move, Move, Move,
                                      Thing, Thing, Thing, Thing, Thing)
frame t (a, b) = (a, b, a', b', t, at, bt, at', bt' )
  where xf = transform a b
        a' = fst xf
        b' = snd xf
        at = move a t
        bt = move b t
        at' = move a' at
        bt' = move b' bt

t0 = Thing 0

f0 = frame t0 (head moves)

next (_, _, _, _, _, _, _, _, t) = frame t

f1 = next f0 $ head $ tail moves
f2 = next f1 $ head $ tail $ tail moves

final = foldl next f2 $ tail $ tail $ tail moves
