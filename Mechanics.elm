module Mechanics where

import List

freeParticleLagrangian : Float -> LocalTuple -> Float
freeParticleLagrangian mass local = 
  let v = velocity local
  in 0.5 * mass * (dotProduct v v)

type LocalTuple = Local Float Vector Vector
velocity : LocalTuple -> Vector
velocity (Local _ _ v) = v

type alias Vector = List Float
dotProduct : Vector -> Vector -> Float
dotProduct v1 v2 = List.foldl (+) 0 <| List.map2 (*) v1 v2