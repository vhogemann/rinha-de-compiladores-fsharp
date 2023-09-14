module Rinha.Interpreter.Call

open Rinha.AST.Nodes

let mapFunction context =
    match context.result with
    | Function _ -> Ok context
    | _ -> Error { description = $"Expecting function but got {context.result}"; location = context.result }

let bindFunction action context =
    match context.result with
    | Function fn -> action fn context
    | _ -> Error { description = $"Expecting function but got {context.result}"; location = context.result }

let mapArgsToParams call context =
    let action fn context =
        fn.parameters
        |> Array.map (fun p -> p.text)
        |> Array.zip call.arguments
        |> Array.fold (fun ctx (value, label) -> ctx |> Context.declare label value) context
        |> Ok
    bindFunction action context

let checkArguments call context =
    let action fn context =
        let args = call.arguments |> Array.length
        let pars = fn.parameters |> Array.length

        if args <> pars then
            Error
                { description = $"Wrong number of arguments: expecting {args} got {pars}"
                  location = Call call }
        else
            Ok context
    bindFunction action context