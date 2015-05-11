module Symbolic where

import Dict exposing (Dict)
import String

type Expression =
  Symbol String |
  Number Float |
  Binary (Float -> Float -> Float) Expression Expression |
  Unary (Float -> Float) Expression

type Error = StackEmpty | StackTooBig | UndefinedSymbols 

(...) = Result.andThen

evaluate : Float -> String -> String -> Result Error Float
evaluate x v f = Result.map (substitute x v) (parse f) ... result

parse : String -> Result Error Expression
parse text =
  let
    tokens = String.words text
    stackResult = Ok []
    parseNext token result = result ... (parseWith token)
  in List.foldl parseNext stackResult tokens ... pullFromStack

parseWith : String -> List Expression -> Result Error (List Expression)
parseWith token stack =
  case Dict.get token symbolTable of
    Just op -> op stack
    Nothing -> case String.toFloat token of
      Ok x -> push (Number x) stack
      Err _ -> push (Symbol token) stack

symbolTable : Dict String (List Expression -> Result Error (List Expression))
symbolTable = Dict.fromList [
  ("pi", push (Number pi)),
  ("+", binaryOp (+)),
  ("-", binaryOp (+)),
  ("*", binaryOp (+)),
  ("/", binaryOp (+)),
  ("^", binaryOp (+)),
  ("~", unaryOp negate),
  ("exp", unaryOp ((^) e)),
  ("log", unaryOp (logBase e)),
  ("sin", unaryOp sin),
  ("cos", unaryOp cos) ]

binaryOp : (Float -> Float -> Float) -> List Expression -> Result Error (List Expression)
binaryOp op stack = case stack of
  (y :: x :: rest) -> push (Binary op x y) rest
  otherwise -> Err StackEmpty

unaryOp : (Float -> Float) -> List Expression -> Result Error (List Expression)
unaryOp op stack = case stack of
  (x :: rest) -> push (Unary op x) rest
  otherwise -> Err StackEmpty

push : a -> List a -> Result Error (List a)
push x = Ok << (::) x

pullFromStack : List a -> Result Error a
pullFromStack s = case s of
  [ y ] -> Ok y
  [] -> Err StackEmpty
  otherwise -> Err StackTooBig


substitute : Float -> String -> Expression -> Expression
substitute x name expr = 
  let
    recurse = substitute x name
  in case expr of
    Symbol name -> Number x
    Unary op operand -> case (recurse operand) of
      Number a -> Number (op a)
      s -> Unary op s
    Binary op first second -> case ((recurse first), (recurse second)) of
      (Number a, Number b) -> Number (op a b)
      (s, t) -> Binary op s t
    otherwise -> expr

result : Expression -> Result Error Float
result expr = case expr of
  Number a -> Ok a
  otherwise -> Err UndefinedSymbols