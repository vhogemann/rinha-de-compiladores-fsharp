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
       let result = Term.eval print (Ok Context.empty)
       test <@ result |> Result.isOk @>
     
    
[<Test>]
let ``Literal`` () =
    let str = Term.Str {
        location = location
        value = "Hello World"
    }
    
    let context = Context.literal str
    
    test <@
        match context.result with
        | Term.Str str ->
            str.value = "Hello World"
        | _ ->
            false
         @>
    
[<Test>]
let ``Sum`` () =
    let json = File.ReadAllText("JSON/sum.json")
    let maybeFile =
        json
        |> Rinha.Parser.parse
    
    match maybeFile with
    | Error jsonError -> failwith jsonError
    | Ok file ->
        match (Term.eval file.expression (Ok Context.empty)) with
        | Error { description = desc ; location = _ } -> failwith desc
        | Ok context ->
            test <@
                    match context.result with
                    | Null _ -> true
                    | _ -> false
                 @>
  