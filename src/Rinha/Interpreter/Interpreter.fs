module Rinha.Interpreter.Eval

open Rinha.AST.Nodes
open Rinha.Interpreter

let rec evaluate (environment:Environment) term : Value =
    let eval = evaluate environment
    match term with
    | Term.Int i ->
        Literal.visitInt i
    | Term.Str s ->
        Literal.visitStr s
    | Term.Bool b ->
        Literal.visitBool b
    | Term.Tuple t ->
        Literal.visitTuple eval t
    | Term.First f ->
        Literal.visitFirst eval f
    | Term.Second s ->
        Literal.visitSecond eval s
    | Term.Print p ->
        Statement.visitPrint eval p
    | Term.Binary bin ->
        Binary.visit eval bin
    | Term.Let l ->
        let env = Statement.visitLet eval environment l
        evaluate env l.next
    | Term.Var v ->
        Literal.visitVar environment v
    | _ ->
        Value.Null
