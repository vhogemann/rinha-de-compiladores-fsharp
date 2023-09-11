module Test.Interpreter

open NUnit.Framework
open Rinha.AST.Nodes
open Swensen.Unquote

let location:Loc = { start = 0; ``end`` = 0; filename = "hello_world"  } 

[<Test>]
let ``Hello World`` () =
    let file:File = {
       name = "hello_world"
       location = location
       expression =
           Term.Print {
                location = location
                value = Term.Str {
                    value = "Hello World"
                    location = location
                }
           }
    }
    ()
[<Test>]
let ``Literal`` () =
    let str = Term.Str {
        location = location
        value = "Hello World"
    }
    
    let context = Rinha.Interpreter.Context.literal str
    
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