module Symbolic where

import Dict exposing (Dict)
import String

type alias Expression = List Symbol
type Symbol = Unknown | Number Float | Plus
type Error = StackEmpty | StackTooBig | BadToken String

(...) = Result.andThen

evaluate v x f = lex x f ... valueAt v

lex : String -> String -> Result Error Expression
lex unknown text = 
  let
    symbolTable = Dict.fromList [("+", Plus), ("1", Number 1), (unknown, Unknown)]
    mergeResult token lexedSymbols = Result.map2 (::) (identify token symbolTable) lexedSymbols 
  in List.foldr mergeResult (Ok []) (String.words text)

identify : String -> Dict String Symbol -> Result Error Symbol
identify token symbols =
  case String.toFloat token of
    Ok f -> Ok (Number f)
    Err _ -> Dict.get token symbols |> Result.fromMaybe (BadToken token)

valueAt : Float -> Expression -> Result Error Float
valueAt x f =
  let
    stackResult = Ok []
    parseNext symbol result = result ... (parseWith x symbol)
  in List.foldl parseNext stackResult f ... pullFromStack

parseWith : Float -> Symbol -> List Float -> Result Error (List Float)
parseWith x c stack = case c of
  Unknown -> Ok <| x :: stack
  Number c -> Ok <| c :: stack
  Plus -> apply (+) stack

apply : (Float -> Float -> Float) -> List Float -> Result Error (List Float)
apply op stack = case stack of
  x :: y :: rest -> Ok <| (op x y) :: rest
  otherwise -> Err StackEmpty

pullFromStack : List Float -> Result Error Float
pullFromStack s = case s of
  [ y ] -> Ok y
  [] -> Err StackEmpty
  otherwise -> Err StackTooBig