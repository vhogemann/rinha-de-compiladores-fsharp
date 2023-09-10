module Test.Interpreter

open NUnit.Framework
open Rinha.AST.Nodes
let ``Hello World`` () =
    let location:Loc = { start = 0; ``end`` = 0; filename = "hello_world"  } 
    let file:File = {
       name = "hello_world"
       location = location
       expression =
           Term.Print {
                kind = "Print"
                location = location
                value = Term.Str {
                    kind = "Str"
                    value = "Hello World"
                    location = location
                }
           }
    }
    ()