module Rinha.Interpreter.Term

open Rinha.Interpreter
open Rinha.AST.Nodes

let rec eval (term:Term) (context:Result<Context,RuntimeError>) =
    match context with
    | Error _ -> context
    | Ok context ->        
        
    match term with
    | Term.Var aVar ->
        aVar
        |> Var.eval context 
    
    | Term.Let node ->
        context
        |> Let.eval node
        |> eval node.next
        
    | Term.Call call ->
       context
       |> Call.checkArguments call
       |> Result.bind (Call.mapArgsToParams call)
       |> eval call.callee
      
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
            