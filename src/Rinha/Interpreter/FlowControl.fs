module Rinha.Interpreter.FlowControl

open Rinha.AST

let visitIf (eval:Evaluator) (aIf:Nodes.If) =
    let result = eval aIf.condition
    match result with
    | Bool condition ->
        if condition then
            eval aIf.``then``
        else
            eval aIf.otherwise
    | _ ->
        Error $"Expecting Bool condition but got {aIf.condition.GetType().Name} @ { aIf.location }"