module Rinha.Interpreter.Call

open Rinha.AST.Nodes

let bind action (call: Call) (context: Context) =
    match call.callee with
    | Function fn -> action call fn context
    | _ ->
        Error
            { description = $"Expecting function but got {call.callee}"
              location = Call call }

let mapArgsToParams =
    let action = fun call fn context-> 
        fn.parameters
        |> Array.map (fun p -> p.text)
        |> Array.zip call.arguments
        |> Array.fold (fun ctx (value, label) -> ctx |> Context.declare label value) context
        |> Ok
    bind action

let checkArguments =
    let action = fun call fn context ->
        let args = call.arguments |> Array.length
        let pars = fn.parameters |> Array.length

        if args <> pars then
            Error
                { description = $"Wrong number of arguments: expecting {args} got {pars}"
                  location = Call call }
        else
            Ok context
    bind action