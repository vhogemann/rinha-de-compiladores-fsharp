module Rinha.Interpreter.Term

open Rinha.Interpreter
open Rinha.AST.Nodes

let rec eval (term:Term) (context:Context) : Context =
    match term with
    | Term.Var node ->
        Var.eval node context
    | Term.Let node ->
        Let.eval eval node context
    | Term.Call call ->
        Call.eval eval call context
    | Term.Binary binary ->
        Binary.eval eval binary context
    | Term.If ``if`` ->
        If.eval eval ``if`` context
    | Term.Print node ->
        Print.eval eval node context
    | Term.First first ->
        Tuple.evalFirst eval first context
    | Term.Second second ->
        Tuple.evalSecond eval second context
    | Term.Null _
    | Term.Bool _
    | Term.Int _
    | Term.Str _
    | Term.Tuple _
    | Term.Function _ ->
        Context.literal term