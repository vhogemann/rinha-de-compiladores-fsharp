module Rinha.Interpreter.Term

open Rinha.Interpreter
open Rinha.AST.Nodes

let rec eval (term: Term) (ctx: Result<Context, RuntimeError>) =
    match ctx with
    | Error _ -> ctx
    | Ok context ->

        match term with
        | Term.Var aVar ->
            aVar
            |> Var.eval context
            |> Result.bind( fun ctx -> eval ctx.result (Ok ctx))

        | Term.Let node -> context |> Let.eval node |> eval node.next

        | Term.Call call ->
            ctx
            |> eval call.callee
            |> Result.bind Call.mapFunction
            |> Result.bind (Call.checkArguments call)
            |> Result.bind (Call.mapArgsToParams call)
            |> Result.bind (Call.bindFunction (fun fn ctx -> eval fn.value (Ok ctx)))

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
            
        | Term.First first ->
            Tuple.first first
            |> Result.bind ( fun term -> eval term ctx )
            
        | Term.Second second ->
            Tuple.second second
            |> Result.bind ( fun term -> eval term ctx )
        
        | Term.Null _
        | Term.Bool _
        | Term.Int _
        | Term.Str _
        | Term.Tuple _
        | Term.Function _ -> Ok { context with result = term }
