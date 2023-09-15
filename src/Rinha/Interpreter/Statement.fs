module Rinha.Interpreter.Statement

open Rinha.AST.Nodes

let visitPrint (eval: Evaluator) (print: Print) =
    let value = eval print.value |> Literal.toString
    printfn $"{value}"
    Value.Null
    
let visitLet (eval: Evaluator) (environment:Environment) (aLet: Let) =
    let value = eval aLet.value
    environment
    |> Map.add aLet.name.text value