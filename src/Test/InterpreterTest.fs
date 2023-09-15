module Test.Interpreter

open NUnit.Framework
open Rinha.AST.Nodes
open Rinha.Interpreter
open Swensen.Unquote

let LOC: Loc =
    { start = 0
      ``end`` = 0
      filename = "hello_world" }

[<Test>]
let ``1 + 1`` () =
    let result =
        Term.Binary
            { lhs = Term.Int { value = 1M; location = LOC }
              op = BinaryOp.Add
              rhs = Term.Int { value = 1M; location = LOC }
              location = LOC }
        |> Eval.evaluate Map.empty

    test
        <@
            match result with
            | Value.Int i -> i = 2M
            | _ -> false
        @>

[<Test>]
let ``1 + ( 2 - 1 )`` () =
    let result =
        Term.Binary
            { lhs = Term.Int { value = 1M; location = LOC }
              op = BinaryOp.Add
              rhs =
                Term.Binary
                    { lhs = Term.Int { value = 2M; location = LOC }
                      op = BinaryOp.Sub
                      rhs = Term.Int { value = 1M; location = LOC }
                      location = LOC }
              location = LOC }
        |> Eval.evaluate Map.empty

    test
        <@
            match result with
            | Value.Int i -> i = 2M
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
        |> Eval.evaluate Map.empty

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
                    { first = Term.Int { value = 0M; location = LOC }
                      second = Term.Int { value = 1M; location = LOC }
                      location = LOC }
              location = LOC }
        |> Eval.evaluate Map.empty

    test
        <@
            match result with
            | Value.Int i -> i = 0M
            | _ -> false
        @>

[<Test>]
let ``second (0,1)`` () =
    let result =
        Term.Second
            { value =
                Term.Tuple
                    { first = Term.Int { value = 0M; location = LOC }
                      second = Term.Int { value = 1M; location = LOC }
                      location = LOC }
              location = LOC }
        |> Eval.evaluate Map.empty

    test
        <@
            match result with
            | Value.Int i -> i = 1M
            | _ -> false
        @>
        
[<Test>]
let ``Variable declaration`` () =
    let result =
        Term.Let {
            name = { text = "a"; location = LOC }
            value = Term.Int { value = 1M; location = LOC }
            location = LOC
            next = Term.Let {
                name = { text = "b"; location = LOC }
                value = Term.Int { value = 1M; location = LOC }
                location = LOC
                next = Term.Binary {
                    lhs = Term.Var { text = "a"; location = LOC }
                    op = BinaryOp.Add
                    rhs = Term.Var { text = "b"; location = LOC }
                    location = LOC
                } 
            } 
        }
        |> Eval.evaluate Map.empty
    
    test
        <@
            match result with
            | Value.Int i -> i = 2M
            | _ -> false
        @>