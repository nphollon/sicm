module Mechanics where

import List

L_freeParticle : Float -> LocalTuple -> Float
L_freeParticle mass local = 
  let v = velocity local
  in 0.5 * mass * (dotProduct v v)

type LocalTuple = Local Float Vector Vector
velocity : LocalTuple -> Vector
velocity (Local _ _ v) = v

type Vector = List Float
dotProduct : Vector -> Vector -> Float
dotProduct = List.map2 (*) >> List.foldl (+) 0