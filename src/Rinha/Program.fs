// For more information see https://aka.ms/fsharp-console-apps
open System
open System.IO

let execute file =
    let result =
        File.ReadAllText file
        |> Rinha.Parser.parse
    match result with
    | Error msg ->
        Console.Error.WriteLine msg
        1
    | Ok file ->
        match Rinha.Interpreter.Eval.evaluate System.Console.Out Map.empty file.expression with
        | Rinha.Interpreter.Value.Error err ->
            Console.Error.WriteLine err
            1
        | _ ->
            0

match Environment.GetCommandLineArgs() with
| [|_; file|] ->
    execute file |> exit
| _
 ->
    exit 0