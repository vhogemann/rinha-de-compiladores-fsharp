module Test.Interpreter

open System.Text
open NUnit.Framework
open Rinha.AST.Nodes
open Rinha.Interpreter
open Swensen.Unquote
open System.IO

let LOC: Loc =
    { start = 0
      ``end`` = 0
      filename = "hello_world" }

[<Test>]
let ``1 + 1`` () =
    let result =
        Term.Binary
            { lhs = Term.Int { value = 1I; location = LOC }
              op = BinaryOp.Add
              rhs = Term.Int { value = 1I; location = LOC }
              location = LOC }
        |> Eval.evaluate System.Console.Out Map.empty

    test
        <@
            match result with
            | Value.Int i -> i = 2I
            | _ -> false
        @>

[<Test>]        
let ``1 - 1`` () =
    let result =
        Term.Binary
            { lhs = Term.Int { value = 1I; location = LOC }
              op = BinaryOp.Sub
              rhs = Term.Int { value = 1I; location = LOC }
              location = LOC }
        |> Eval.evaluate System.Console.Out Map.empty

    test
        <@
            match result with
            | Value.Int i -> i = 0I
            | _ -> false
        @>
        
[<Test>]        
let ``1 / 1`` () =
    let result =
        Term.Binary
            { lhs = Term.Int { value = 1I; location = LOC }
              op = BinaryOp.Div
              rhs = Term.Int { value = 1I; location = LOC }
              location = LOC }
        |> Eval.evaluate System.Console.Out Map.empty

    test
        <@
            match result with
            | Value.Int i -> i = 1I
            | _ -> false
        @>
        
[<Test>]        
let ``1 / 0`` () =
    let result =
        Term.Binary
            { lhs = Term.Int { value = 1I; location = LOC }
              op = BinaryOp.Div
              rhs = Term.Int { value = 0I; location = LOC }
              location = LOC }
        |> Eval.evaluate System.Console.Out Map.empty

    test
        <@
            match result with
            | Value.Error _ -> true
            | _ -> false
        @>

[<Test>]
let ``1 + ( 2 - 1 )`` () =
    let result =
        Term.Binary
            { lhs = Term.Int { value = 1I; location = LOC }
              op = BinaryOp.Add
              rhs =
                Term.Binary
                    { lhs = Term.Int { value = 2I; location = LOC }
                      op = BinaryOp.Sub
                      rhs = Term.Int { value = 1I; location = LOC }
                      location = LOC }
              location = LOC }
        |> Eval.evaluate System.Console.Out Map.empty

    test
        <@
            match result with
            | Value.Int i -> i = 2I
            | _ -> false
        @>

[<Test>]
let ``"Hello" + " world!"`` () =
    let result =
        Term.Binary
            { lhs = Term.Str { value = "Hello"; location = LOC }
              op = BinaryOp.Add
              rhs = Term.Str { value = " world!"; location = LOC }
              location = LOC }
        |> Eval.evaluate System.Console.Out Map.empty

    test
        <@
            match result with
            | Value.Str s -> s = "Hello world!"
            | _ -> false
        @>

[<Test>]
let ``first (0,1)`` () =
    let result =
        Term.First
            { value =
                Term.Tuple
                    { first = Term.Int { value = 0I; location = LOC }
                      second = Term.Int { value = 1I; location = LOC }
                      location = LOC }
              location = LOC }
        |> Eval.evaluate System.Console.Out Map.empty

    test
        <@
            match result with
            | Value.Int i -> i = 0I
            | _ -> false
        @>

[<Test>]
let ``second (0,1)`` () =
    let result =
        Term.Second
            { value =
                Term.Tuple
                    { first = Term.Int { value = 0I; location = LOC }
                      second = Term.Int { value = 1I; location = LOC }
                      location = LOC }
              location = LOC }
        |> Eval.evaluate System.Console.Out Map.empty

    test
        <@
            match result with
            | Value.Int i -> i = 1I
            | _ -> false
        @>

[<Test>]
let ``Variable declaration`` () =
    // let a = 1
    // let b = 1
    // let c = a + b
    // c
    let result =
        Term.Let
            { name = { text = "a"; location = LOC }
              value = Term.Int { value = 1I; location = LOC }
              location = LOC
              next =
                Term.Let
                    { name = { text = "b"; location = LOC }
                      value = Term.Int { value = 1I; location = LOC }
                      location = LOC
                      next =
                        Term.Let
                            { name = { text = "c"; location = LOC }
                              value =
                                Term.Binary
                                    { lhs = Term.Var { text = "a"; location = LOC }
                                      op = BinaryOp.Add
                                      rhs = Term.Var { text = "b"; location = LOC }
                                      location = LOC }
                              location = LOC
                              next = Var { text = "c"; location = LOC } } } }
        |> Eval.evaluate System.Console.Out Map.empty

    test
        <@
            match result with
            | Value.Int i -> i = 2I
            | _ -> false
        @>

[<Test>]
let ``If Then`` () =
    let result =
        Term.If
            { condition =
                Term.Binary
                    { lhs = Term.Int { value = 1I; location = LOC }
                      op = BinaryOp.Gte
                      rhs = Term.Int { value = 0I; location = LOC }
                      location = LOC }
              ``then`` = Term.Str { value = "foo"; location = LOC }
              otherwise = Term.Str { value = "bar"; location = LOC }
              location = LOC }
        |> Eval.evaluate System.Console.Out Map.empty

    test
        <@
            match result with
            | Str s -> s = "foo"
            | _ -> false
        @>

[<Test>]
let ``If Else`` () =
    let result =
        Term.If
            { condition =
                Term.Binary
                    { lhs = Term.Int { value = 0I; location = LOC }
                      op = BinaryOp.Gte
                      rhs = Term.Int { value = 1I; location = LOC }
                      location = LOC }
              ``then`` = Term.Str { value = "foo"; location = LOC }
              otherwise = Term.Str { value = "bar"; location = LOC }
              location = LOC }
        |> Eval.evaluate System.Console.Out Map.empty

    test
        <@
            match result with
            | Str s -> s = "bar"
            | _ -> false
        @>

[<Test>]
let ``Function Call`` () =
    // let sum = (a,b) -> a+b
    // sum(1,1)
    let result =
        Term.Let
            { location = LOC
              name = { text = "sum"; location = LOC }
              value =
                Term.Function
                    { parameters = [| { text = "a"; location = LOC }; { text = "b"; location = LOC } |]
                      value =
                        Term.Binary
                            { lhs = Term.Var { text = "a"; location = LOC }
                              op = BinaryOp.Add
                              rhs = Term.Var { text = "b"; location = LOC }
                              location = LOC }
                      location = LOC }
              next =
                Term.Call
                    { callee = Var { text = "sum"; location = LOC }
                      arguments =
                        [| Term.Int { value = 1I; location = LOC }
                           Term.Int { value = 1I; location = LOC } |]
                      location = LOC } }
        |> Eval.evaluate System.Console.Out Map.empty

    test
        <@
            match result with
            | Value.Int i -> i = 2I
            | _ -> false
        @>

[<Test>]
let ``sum.json`` () =
    let sb = StringBuilder()
    let writer = new StringWriter(sb)
    
    let json = File.ReadAllText("JSON/sum.json")
    let result = json |> Rinha.Parser.parse

    match result with
    | Result.Error msg -> failwith msg
    | Result.Ok file ->
        let result = file.expression |> Eval.evaluate writer Map.empty

        test
            <@
                match result with
                | Value.Null -> sb.ToString().Trim() = "15"
                | _ -> false
            @>

[<Test>]
let ``fib.json`` () =
    let sb = StringBuilder()
    let writer = new StringWriter(sb)
    
    let json = File.ReadAllText("JSON/fib.json")
    let result = json |> Rinha.Parser.parse

    match result with
    | Result.Error msg -> failwith msg
    | Result.Ok file ->
        let result = file.expression |> Eval.evaluate writer Map.empty

        test
            <@
                match result with
                | Value.Null -> sb.ToString().Trim() = "55"
                | _ -> false
            @>


[<Test>]
let ``combination.json`` () =
    let sb = StringBuilder()
    let writer = new StringWriter(sb)
    
    let json = File.ReadAllText("JSON/combination.json")
    let result = json |> Rinha.Parser.parse

    match result with
    | Result.Error msg -> failwith msg
    | Result.Ok file ->
        let result = file.expression |> Eval.evaluate writer Map.empty

        test
            <@
                match result with
                | Value.Null -> sb.ToString().Trim() = "45"
                | _ -> false
            @>

[<Test>]
let ``source.rinha.json`` () =
    let sb = StringBuilder()
    let writer = new StringWriter(sb)
    
    let json = File.ReadAllText("JSON/source.rinha.json")
    let result = json |> Rinha.Parser.parse

    match result with
    | Result.Error msg -> failwith msg
    | Result.Ok file ->
        let result = file.expression |> Eval.evaluate writer Map.empty

        test
            <@
                match result with
                | Value.Null -> sb.ToString() = "@!compile::\r\n@!fibbo::610\r\n"
                | _ -> false
            @>