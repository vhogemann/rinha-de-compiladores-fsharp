module Rinha.Interpreter.Literal

open Rinha.AST
open Rinha.AST.Nodes
open Rinha.Interpreter

let rec toString value : string =
    match value with
    | Value.Str s -> s
    | Value.Int i -> string i
    | Value.Bool b -> string b
    | Value.Error e -> e
    | Value.Fun term -> $"%A{term}"
    | Value.Tuple (left,right) -> $"( { toString left }, { toString right } )"
    | Value.Null -> "Null"

let visitStr (str: Nodes.Str) : Value = str.value |> Value.Str

let visitInt (int: Nodes.Int) : Value = int.value |> Value.Int

let visitBool (bool: Nodes.Bool) : Value = bool.value |> Value.Bool

let visitTuple (eval: Evaluator) (tuple: Nodes.Tuple) : Value =
    let first = eval tuple.first
    let second = eval tuple.second
    (first, second) |> Value.Tuple
    
let visitFirst (eval: Evaluator) (first: Nodes.First) =
    let value = eval first.value

    match value with
    | Value.Tuple(first, _) -> first
    | Value.Error _ -> value
    | _ -> Error $"Expecting a Tuple but got a {value.GetType().Name} @ {first.location}"


let visitSecond (eval: Evaluator) (second: Nodes.Second) =
    let value = eval second.value
    
    match value with
    | Value.Tuple(_, second) -> second
    | Value.Error _ -> value
    | _ -> Error $"Expecting a Tuple but got a {value.GetType().Name} @ {second.location}"
    
let visitVar (environment:Environment) (var:Var) =
    if environment |> Map.containsKey var.text then
        environment.[var.text]
    else
        Null
       