module Symbolic where

import Dict exposing (Dict)
import String

type Expression =
  Unknown |
  Number Float |
  Binary BinaryOp Expression Expression |
  Unary UnaryOp Expression

type BinaryOp = Plus | Minus | Times | Over | Power
type UnaryOp = Exp | Log | Sin | Cos

type Error =
  StackEmpty |
  StackTooBig |
  UnparseableSymbol |
  NotDifferentiable

(...) = Result.andThen

evaluate : String -> Float -> Result Error Float
evaluate formula x0 = parse formula |> Result.map (valueAt x0)

valueAt : Float -> Expression -> Float
valueAt x expr =
  let r = valueAt x 
  in case expr of
    Unknown -> x
    Number c -> c
    Binary op a b -> applyBinary op (r a) (r b)
    Unary op a -> applyUnary op (r a)

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
      Err _ -> Err UnparseableSymbol

symbolTable : Dict String (List Expression -> Result Error (List Expression))
symbolTable = Dict.fromList [
  ("x", push (Unknown)),
  ("pi", push (Number pi)),
  ("+", binaryOpStack plus'),
  ("-", binaryOpStack minus'),
  ("*", binaryOpStack times'),
  ("/", binaryOpStack over'),
  ("^", binaryOpStack power'),
  ("exp", unaryOpStack exp'),
  ("log", unaryOpStack log'),
  ("sin", unaryOpStack sin'),
  ("cos", unaryOpStack cos') ]

binaryOpStack : (Expression -> Expression -> Expression) -> List Expression -> Result Error (List Expression)
binaryOpStack op stack = case stack of
  (y :: x :: rest) -> push (op x y) rest
  otherwise -> Err StackEmpty

unaryOpStack : (Expression -> Expression) -> List Expression -> Result Error (List Expression)
unaryOpStack op stack = case stack of
  (x :: rest) -> push (op x) rest
  otherwise -> Err StackEmpty

push : a -> List a -> Result Error (List a)
push x = Ok << (::) x

pullFromStack : List a -> Result Error a
pullFromStack s = case s of
  [ y ] -> Ok y
  [] -> Err StackEmpty
  otherwise -> Err StackTooBig

differentiate : Expression -> Expression
differentiate expr =
  let d f = (f, differentiate f)
  in case expr of
    Unknown -> Number 1
    Number a -> Number 0
    Binary op f g -> binaryDerivative op (d f) (d g)
    Unary op f -> unaryDerivative op (d f)

binaryDerivative : BinaryOp -> (Expression, Expression) -> (Expression, Expression) -> Expression
binaryDerivative op (g, dg) (h, dh) = case op of
  Plus -> plus' dg dh
  Minus -> minus' dg dh
  Times -> plus' (times' h dg) (times' dh g)
  Over -> minus' (over' dg h) (times' g (over' dh (power' h (Number 2))))
  Power -> times' (power' g h) (plus' (times' dh (log' g)) (times' h (over' dg g)))

unaryDerivative : UnaryOp -> (Expression, Expression) -> Expression
unaryDerivative op (g, dg) = case op of
  Exp -> times' dg (exp' g)
  Log -> over' dg g
  Sin -> times' dg (cos' g)
  Cos -> times' (Number -1) (times' dg (sin' g)) 

plus' a b = case (a, b) of
  (Number 0, b') -> b'
  (a', Number 0) -> a'
  otherwise -> simplifyBinary Plus a b

minus' a b = case (a, b) of
  (Number 0, b') -> Binary Times (Number -1) b'
  (a', Number 0) -> a'
  otherwise -> simplifyBinary Minus a b

times' a b = case (a, b) of
  (Number 0, _) -> Number 0
  (_, Number 0) -> Number 0
  (Number 1, b') -> b'
  (a', Number 1) -> a'
  otherwise -> simplifyBinary Times a b

over' a b = case (a, b) of
  (Number 0, _) -> Number 0
  (a', Number 1) -> a'
  otherwise -> simplifyBinary Over a b

power' a b = case (a, b) of
  (a', Number 0) -> Number 1
  (Number 1, _) -> Number 1
  (a', Number 1) -> a'
  otherwise -> simplifyBinary Power a b

exp' = simplifyUnary Exp
log' = simplifyUnary Log
sin' = simplifyUnary Sin
cos' = simplifyUnary Cos

simplifyBinary : BinaryOp -> Expression -> Expression -> Expression
simplifyBinary op a b = case (a, b) of
  (Number x, Number y) -> Number (applyBinary op x y)
  otherwise -> Binary op a b

applyBinary : BinaryOp -> Float -> Float -> Float
applyBinary op = case op of
  Plus -> (+)
  Minus -> (-)
  Times -> (*)
  Over -> (/)
  Power -> (^)

simplifyUnary : UnaryOp -> Expression -> Expression
simplifyUnary op a = case a of
  Number x -> Number (applyUnary op x)
  otherwise -> Unary op a

applyUnary : UnaryOp -> Float -> Float
applyUnary op = case op of
  Exp -> (^) e
  Log -> logBase e
  Sin -> sin
  Cos -> cos