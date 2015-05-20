module Mechanics where

import List
import Result

import Symbolic

freeParticleLagrangian : Float -> LocalTuple -> Float
freeParticleLagrangian mass local = 
  let v = velocity local
  in 0.5 * mass * (square v)

type LocalTuple = Local Float Vector Vector
velocity : LocalTuple -> Vector
velocity (Local _ _ v) = v

type alias Vector = List Float

dim : Vector -> Int
dim = List.length

vectorPair : Vector -> Vector -> Result String (Vector, Vector)
vectorPair v1 v2 =
  let pair = (v1, v2)
  in if | dim v1 == dim v2 -> Ok pair
        | otherwise -> Err <| "Vectors are not the same size: " ++ toString pair

dotProduct : (Vector, Vector) -> Float
dotProduct (v1, v2) = List.foldl (+) 0 <| List.map2 (*) v1 v2

square : Vector -> Float
square = List.foldl (\x y -> x^2 + y) 0

q : List Symbolic.Expression -> Float -> Vector
q coords t = List.map (Symbolic.valueAt t) coords
