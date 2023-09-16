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
    | Term.Function aFun -> Callable.visitFun aFun
    | Term.Call aCall ->
        let result = eval aCall.callee
        match result with
        | Value.Fun func ->
            if Callable.argsMatch aCall func then
                let env = Callable.mapArguments eval environment aCall func
                evaluate env func.value
            else
                Error "arguments mismatch"
        | _ ->
            Error $"Expecting a Function but got a {result.GetType().Name} @ ${ aCall.location }"
    | _ -> Value.Null
