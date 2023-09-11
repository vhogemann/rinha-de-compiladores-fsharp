module Rinha.Interpreter.Call

open Rinha.AST.Nodes

let isFunction (term:Term) =
    match term with
    | Term.Function fn -> Ok fn
    | _ -> Error { description = "Expected a function"; location = term }

let resolve (arguments:Result<Term,RuntimeError>[]) =
    arguments
    |> Array.fold (fun state args ->
        match args with
        | Ok argument -> fst state |> Array.append [| argument |], snd state
        | Error error -> fst state, snd state |> Array.append [| error |]
        ) ([||],[||])
    |> fun (args,errors) ->
        if errors |> Array.isEmpty then
            Ok args
        else
            Error errors.[0]
            
let execute (evaluator:Eval) (fn:Function) (call:Call) (context:Context) =
    let paramLenght = fn.parameters |> Array.length
    let argLenght = call.arguments |> Array.length
    if paramLenght <> argLenght then
        Error { description = $"Expected %d{paramLenght} arguments but got %d{argLenght}"
                location = Term.Null }
    else
        let paramNames = fn.parameters |> Array.map (fun p -> p.text)
        
        call.arguments
        |> Array.map (fun arg -> evaluator arg context)
        |> Array.map Context.result
        |> resolve
        |> Result.map (fun args ->
            let locals =
                context.locals
                |> Map.toArray
                |> Array.append (args |> Array.zip paramNames)
                |> Map.ofArray
            { context with locals = locals }
        )
        |> Result.bind (fun context ->
            context
            |> evaluator fn.value
            |> Context.result
        )

let eval (evaluator:Eval) (term:Call) (context:Context) =
    let result =
        context |> evaluator term.callee
        |> Context.result
        |> Result.bind isFunction
        |> Result.bind (fun fn -> execute evaluator fn term context)
    context |> Context.withResult result
