module Rinha.Interpreter.Tuple

open Rinha.AST.Nodes

let evalFirst (evaluator:Eval) (term:First) (context:Context) =
    match term.value with
    | Tuple tuple -> evaluator tuple.first context
    | _ ->
        let error = Error { description = $"Expecting a Tuple but got a {term.value}"; location = term.value }
        context |> Context.withResult error
    
let evalSecond (evaluator:Eval) (term:Second) (context:Context) =
    match term.value with
    | Tuple tuple -> evaluator tuple.second context
    | _ ->
        let error = Error { description = $"Expecting a Tuple but got a {term.value}"; location = term.value }
        context |> Context.withResult error