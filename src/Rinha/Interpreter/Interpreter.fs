module Rinha.Interpreter.Eval

open Rinha.AST.Nodes
open Rinha.Interpreter

let rec evaluate (environment: Environment) term : Value =
    let eval = evaluate environment

    match term with
    | Term.Int aInt -> Literal.visitInt aInt
    | Term.Str aStr -> Literal.visitStr aStr
    | Term.Bool aBool -> Literal.visitBool aBool
    | Term.Tuple aTuple -> Literal.visitTuple eval aTuple
    | Term.First aFirst -> Literal.visitFirst eval aFirst
    | Term.Second aSecond -> Literal.visitSecond eval aSecond
    | Term.Print aPrint -> Statement.visitPrint eval aPrint
    | Term.Binary aBinary -> Binary.visit eval aBinary
    | Term.Let aLet ->
        let env = Statement.visitLet eval environment aLet
        evaluate env aLet.next
    | Term.Var aVar -> Literal.visitVar environment aVar
    | Term.If aIf -> FlowControl.visitIf eval aIf
    | _ -> Value.Null
