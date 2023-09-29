module Rinha.Interpreter.Binary

open System.Numerics
open Rinha.Interpreter
open Rinha.AST.Nodes

let visitStr left right binary =
    let right = Literal.toString right

    match binary.op with
    | BinaryOp.Add -> Value.Str(left + right)
    | BinaryOp.Eq -> Value.Bool(left = right)
    | BinaryOp.Neq -> Value.Bool(left <> right)
    | _ -> Value.Error $"Unsupported operation {binary.op} for type Str @ %A{binary.location}"

let visitInt (left:BigInteger) (right:BigInteger) binary =
    match binary.op with
    | Add -> Value.Int(left + right)
    | Sub -> Value.Int(left - right)
    | Mul -> Value.Int(left * right)
    | Div ->
        if right = 0 then
            Value.Error $"Division by zero @ %A{binary.location}"
        else
            Value.Int(left / right)
    | Rem -> Value.Int(left % right)
    | Eq -> Value.Bool(left = right)
    | Neq -> Value.Bool(left <> right)
    | Lt -> Value.Bool(left < right)
    | Gt -> Value.Bool(left > right)
    | Lte -> Value.Bool(left <= right)
    | Gte -> Value.Bool(left >= right)
    | _ -> Value.Error $"Unsupported operation {binary.op} for type Int @ %A{binary.location}"

let visitBool left right binary =
    match binary.op with
    | And -> Value.Bool(left && right)
    | Eq -> Value.Bool(left = right)
    | Neq -> Value.Bool(left <> right)
    | Or -> Value.Bool(left || right)
    | Not -> Value.Bool(not right)
    | _ -> Value.Error $"Unsupported operation {binary.op} for type Bool @ %A{binary.location}"

let visit (eval: Evaluator) (bin: Binary) : Value =
    let left = bin.lhs |> eval
    let right = bin.rhs |> eval

    match left, right with
    | Value.Str s, _ -> visitStr s right bin
    | Value.Int l, Value.Int r -> visitInt l r bin
    | Value.Bool l, Value.Bool r -> visitBool l r bin
    | Error e, _ -> Error e
    | _, Error e -> Error e
    | _, _ ->
        Value.Error
            $"Unsupported operation {bin.op} for types {left.GetType().Name} and {right.GetType().Name} @ %A{bin.location}"
