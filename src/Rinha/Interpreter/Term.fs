module Rinha.Interpreter.Term

open Rinha.Interpreter
open Rinha.AST.Nodes

let rec eval (term: Term) (ctx: Result<Context, RuntimeError>) =
    match ctx with
    | Error _ -> ctx
    | Ok context ->

        match term with
        | Term.Var aVar -> aVar |> Var.eval context

        | Term.Let node -> context |> Let.eval node |> eval node.next

        | Term.Call call ->
            context
            |> Call.checkArguments call
            |> Result.bind (Call.mapArgsToParams call)
            |> eval call.callee

        | Term.Binary binary ->
            ctx
            |> eval binary.lhs
            |> Result.map (fun context -> context.result)
            |> Result.bind (fun lhs -> ctx |> eval binary.rhs |> Result.map (fun rhs -> lhs, rhs))
            |> Result.bind (fun (lhs, rhs) -> Binary.execute lhs rhs.result binary)
            |> Result.map (fun result -> { context with result = result })
        
        | Term.If aIf ->
            ctx
            |> eval aIf.condition
            |> Result.bind (fun context ->
                match context.result with
                | Bool b ->
                    if b.value then
                        eval aIf.``then`` ctx
                    else
                        eval aIf.otherwise ctx
                | _ ->
                    Error
                        { description = $"Expecting Bool but got {context.result}"
                          location = context.result })
        
        | Term.Print node ->
            match ctx |> eval node.value with
            | Error err -> printfn $"%A{err}"
            | Ok context -> printfn $"%A{context.result}"            
            ctx
            
        | Term.First first -> Tuple.evalFirst eval first context
        | Term.Second second -> Tuple.evalSecond eval second context
        | Term.Null _
        | Term.Bool _
        | Term.Int _
        | Term.Str _
        | Term.Tuple _
        | Term.Function _ -> Context.literal term
