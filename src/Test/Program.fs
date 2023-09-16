module Program =
    open System.IO
    [<EntryPoint>]
    let main _ =
        let result =
            File.ReadAllText "/var/rinha/source.rinha.json"
            |> Rinha.Parser.parse
        match result with
        | Error msg ->
            System.Console.Error.WriteLine msg
            1
        | Ok file ->
            Rinha.Interpreter.Eval.evaluate Map.empty file.expression |> ignore
            0
