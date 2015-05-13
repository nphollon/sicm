module Symbolic where

import Dict exposing (Dict)
import String

type Expression =
  Symbol String |
  Number Float |
  Binary BinaryOp Expression Expression |
  Unary UnaryOp Expression

type BinaryOp = Plus | Minus | Times | Over | Power
type UnaryOp = Exp | Log | Sin | Cos

type Error =
  StackEmpty |
  StackTooBig |
  UndefinedSymbols |
  NotDifferentiable


symbolTable : Dict String (List Expression -> Result Error (List Expression))
symbolTable = Dict.fromList [
  ("pi", push (Number pi)),
  ("+", binaryOp Plus),
  ("-", binaryOp Minus),
  ("*", binaryOp Times),
  ("/", binaryOp Over),
  ("^", binaryOp Power),
  ("exp", unaryOp Exp),
  ("log", unaryOp Log),
  ("sin", unaryOp Sin),
  ("cos", unaryOp Cos) ]

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

binaryOp : BinaryOp -> List Expression -> Result Error (List Expression)
binaryOp op stack = case stack of
  (y :: x :: rest) -> push (Binary op x y) rest
  otherwise -> Err StackEmpty

unaryOp : UnaryOp -> List Expression -> Result Error (List Expression)
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
    Symbol s -> if (s == name) then (Number x) else expr
    Unary op operand -> Unary op (recurse operand)
    Binary op first second -> Binary op (recurse first) (recurse second)
    otherwise -> expr

simplify : Expression -> Expression
simplify expr = 
  case expr of
    Binary op f g -> simplifyBinary op (simplify f) (simplify g)
    Unary op f -> simplifyUnary op (simplify f)
    otherwise -> expr

simplifyUnary : UnaryOp -> Expression -> Expression
simplifyUnary op f = case (op, f) of
  (Exp, Number a) -> Number (e ^ a)
  (Log, Number a) -> Number (logBase e a)
  (Sin, Number a) -> Number (sin a)
  (Cos, Number a) -> Number (cos a)
  otherwise -> Unary op f


simplifyBinary : BinaryOp -> Expression -> Expression -> Expression
simplifyBinary op f g = case (op, f, g) of
  (Plus, Number 0, _) -> g
  (Plus, _, Number 0) -> f
  (Plus, Number a, Number b) -> Number (a + b)

  (Minus, _, _) -> simplify (Binary Plus f (Binary Times g (Number -1)))

  (Times, Number 0, _) -> Number 0
  (Times, _, Number 0) -> Number 0
  (Times, Number 1, _) -> g
  (Times, _, Number 1) -> f
  (Times, Number a, Number b) -> Number (a * b)

  (Over, _, _) -> simplify (Binary Times f (Binary Power g (Number -1)))

  (Power, _, Number 0) -> Number 1
  (Power, Number 1, _) -> Number 1
  (Power, _, Number 1) -> f
  (Power, Number a, Number b) -> Number (a ^ b)

  otherwise -> Binary op f g

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