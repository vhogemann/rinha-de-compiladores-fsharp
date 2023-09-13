module Rinha.Interpreter.Term

open Rinha.Interpreter
open Rinha.AST.Nodes

let eval (term:Term) (context:Context)  : Context =
    let rec evaluate term context=
        match term with
        | Term.Var aVar ->
            aVar
            |> Var.eval context 
        | Term.Let node ->
            node
            |> Let.eval context
            |> evaluate node.next
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
            
    Context.empty
