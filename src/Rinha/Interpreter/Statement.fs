module Rinha.Interpreter.Statement

open System.IO
open Rinha.AST.Nodes

let visitPrint (eval: Evaluator) (out:TextWriter) (print: Print) =
    let value = eval print.value |> Literal.toString
    out.WriteLine $"{value}"
    Value.Null
    
let visitLet (eval: Evaluator) (environment:Environment) (aLet: Let) =
    let value = eval aLet.value
    environment
    |> Map.add aLet.name.text value