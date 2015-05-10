module Symbolic where

type alias LiteralFunction = List Symbol
type Symbol = Unknown | Number Float | Plus
type ParseError = StackEmpty | StackTooBig

{-}
valueAt : Float -> LiteralFunction -> Result Float
valueAt x = Result.andThen pullFromStack << List.foldl (Result.andThen << parseWith x) []
-}

parseWith : Float -> Symbol -> List Float -> Result ParseError (List Float)
parseWith x c stack = case c of
  Unknown -> Ok <| x :: stack
  Number c -> Ok <| c :: stack
  Plus -> apply (+) stack

apply : (Float -> Float -> Float) -> List Float -> Result ParseError (List Float)
apply op stack = case stack of
  x :: y :: rest -> Ok <| (op x y) :: rest
  otherwise -> Err StackEmpty

pullFromStack : List Float -> Result ParseError Float
pullFromStack s = case s of
  [ y ] -> Ok y
  [] -> Err StackEmpty
  otherwise -> Err StackTooBig