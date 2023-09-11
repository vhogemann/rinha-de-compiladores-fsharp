module Rinha.Interpreter.If

open Rinha.AST.Nodes
let eval (evaluator:Eval) (term:If) (context:Context) =
    let condition = evaluator term.condition context
    let result =
        condition.result
        |> Result.bind(fun result ->
            match result with
            | Bool value -> Ok value
            | term ->
                Error { description = $"Expecting Bool but found {condition.result}"; location = term } 
            )
        |> Result.bind(fun result ->
            if result.value then
                evaluator term.``then`` context |> Context.result
            else
                evaluator term.otherwise context |> Context.result)
    context
    |> Context.withResult result