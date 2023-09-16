module Rinha.Interpreter.Callable

open Rinha.AST

let visitFun (func: Nodes.Function) = Value.Fun func

let argsMatch (call: Nodes.Call) (func: Nodes.Function) =
    let aLength = call.arguments |> Array.length
    let pLength = func.parameters |> Array.length
    aLength = pLength

let mapArguments (eval: Evaluator) (environment: Environment) (call: Nodes.Call) (func: Nodes.Function) : Environment =
    let names = func.parameters |> Array.map (fun var -> var.text)
    let values = call.arguments |> Array.map eval

    Array.zip names values
    |> Array.fold (fun state (name, value) -> state |> Map.change name (fun _ -> Some value)) environment
