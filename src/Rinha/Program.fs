// For more information see https://aka.ms/fsharp-console-apps
open System.IO

let result =
    File.ReadAllText "/var/rinha/source.rinha.json"
    |> Rinha.Parser.parse
match result with
| Error msg ->
    System.Console.Error.WriteLine msg
    exit 1
| Ok file ->
    match Rinha.Interpreter.Eval.evaluate System.Console.Out Map.empty file.expression with
    | Rinha.Interpreter.Value.Error err ->
        System.Console.Error.WriteLine err
        exit 1
    | _ ->
        exit 0
