module Symbolic where

import Dict exposing (Dict)
import String

type Expression =
  Symbol String |
  Number Float |
  Binary (Float -> Float -> Float) Expression Expression |
  Unary (Float -> Float) Expression

type Error =
  StackEmpty |
  StackTooBig |
  UndefinedSymbols |
  NotDifferentiable

(...) = Result.andThen

evaluate : Float -> String -> String -> Result Error Float
evaluate x v f = Result.map (substitute x v >> simplify) (parse f) ... result

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
  ("-", binaryOp (-)),
  ("*", binaryOp (+)),
  ("/", binaryOp (+)),
  ("^", binaryOp (+)),
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

-- TODO create simply function that
-- reduces operations involving identities (multiplying by 1 or 0, or adding 0)
-- associativity (a * (b + c) -> a*b + a*c)

substitute : Float -> String -> Expression -> Expression
substitute x name expr = 
  let
    recurse = substitute x name
  in case expr of
    Symbol name -> Number x
    Unary op operand -> Unary op (recurse operand)
    Binary op first second -> Binary op (recurse first) (recurse second)
    otherwise -> expr

simplify : Expression -> Expression
simplify expr = 
  case expr of
    Unary op f -> case simplify f of
      Number c -> Number (op c)
      f' -> Unary op f'
    Binary op f g -> case (simplify f, simplify g) of
      (Number c, Number d) -> Number (op c d)
      (f', g') -> Binary op f' g'
    otherwise -> expr

result : Expression -> Result Error Float
result expr = case expr of
  Number a -> Ok a
  otherwise -> Err UndefinedSymbols

{-}
differentiate : String -> Expression -> Result Error Expression
differentiate symbol expr =
  let
    recurse f = differentiate symbol f
  in case expr of
  Number a -> Ok (Number 0)
  Symbol symbol -> Ok (Number 1)
  Symbol _ -> Ok (Number 0)
  Binary (+) f g -> Result.map2 (Binary (+)) (recurse f) (recurse g) |> Ok
  otherwise -> Err NotDifferentiable
  -}