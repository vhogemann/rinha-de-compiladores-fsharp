module Rinha.Interpreter.Tuple

open Rinha.AST.Nodes

let first (term:First) =
    match term.value with
    | Tuple tuple ->
        Ok tuple.first
    | _ ->
        Error { description = $"Expecting a Tuple but got a {term.value}"; location = term.value }
        
    
let second (term:Second) =
    match term.value with
    | Tuple tuple ->
        Ok tuple.second
    | _ ->
        Error { description = $"Expecting a Tuple but got a {term.value}"; location = term.value }
        