module Test

open NUnit.Framework
open System.IO
open Swensen.Unquote
[<SetUp>]
let Setup () =
    ()

[<Test>]
let ``Parse fib.json`` () =
    let json = File.ReadAllText("JSON/fib.json")
    let result = json |> Rinha.Parser.parse
    test <@ result |> Result.isOk  @>
    
    
[<Test>]
let ``Parse sum.json`` () =
    let json = File.ReadAllText("JSON/sum.json")
    let result = json |> Rinha.Parser.parse
    test <@ result |> Result.isOk  @>