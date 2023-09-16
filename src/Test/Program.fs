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
            match Rinha.Interpreter.Eval.evaluate System.Console.Out Map.empty file.expression with
            | Rinha.Interpreter.Value.Error err ->
                System.Console.Error.WriteLine err
                1
            | _ ->
                0
