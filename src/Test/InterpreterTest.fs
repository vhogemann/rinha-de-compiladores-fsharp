module Test.Interpreter

open System.IO
open NUnit.Framework
open Rinha.AST.Nodes
open Rinha.Interpreter
open Swensen.Unquote

let location:Loc = { start = 0; ``end`` = 0; filename = "hello_world"  } 

[<Test>]
let ``Hello World`` () =
       let print = 
           Term.Print {
                location = location
                value = Term.Str {
                    value = "Hello World"
                    location = location
                }
           }
       let result = Term.eval print (Context.empty)
       test <@ result.result |> Result.isOk @>
     
    
[<Test>]
let ``Literal`` () =
    let str = Term.Str {
        location = location
        value = "Hello World"
    }
    
    let context = Context.literal str
    
    test <@
        match context.result with
        | Ok value ->
            match value with
            | Term.Str str ->
                str.value = "Hello World"
            | _ ->
                false
        | _ ->
            false
         @>
    
[<Test>]
let ``Fib`` () =
    let json = File.ReadAllText("JSON/sum.json")
    let maybeFile =
        json
        |> Rinha.Parser.parse
    
        
    test <@
         match maybeFile with
         | Error _ -> false
         | Ok file ->
            let context = Term.eval file.expression Context.empty
            match context.result with
            | Error _ -> false
            | Ok value ->
                match value with
                | Term.Int num ->
                    num.value = 55M
                | _ ->
                    false
         @>
  