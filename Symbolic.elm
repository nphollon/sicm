module Symbolic where

import Dict exposing (Dict)
import String

type Expression =
  Symbol String |
  Number Float |
  Binary (Float -> Float -> Float) Expression Expression |
  Unary (Float -> Float) Expression

type Error = StackEmpty | StackTooBig

(...) = Result.andThen

--evaluate : Float -> String -> String -> Float
--evaluate v x f = lex f ... parse ... substitute x v ... reduce

lex : String -> Result Error (List String)
lex text = Ok (String.words text)

parse : List String -> Result Error Expression
parse tokens =
  let
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