module Rinha.Parser

open Thoth.Json.Net

module Decode =

    let location (get: Decode.IGetters) =
        let loc: Decoder<AST.Nodes.Loc> =
            Decode.object (fun get ->
                { start = get.Required.Field "start" Decode.int
                  ``end`` = get.Required.Field "end" Decode.int
                  filename = get.Required.Field "filename" Decode.string })

        get.Required.Field "location" loc

    let integer (get: Decode.IGetters) =
        AST.Nodes.Term.Int
            { location = get |> location
              value = get.Required.Field "value" Decode.int64 }

    let string (get: Decode.IGetters) =
        AST.Nodes.Term.Str
            { location = get |> location
              value = get.Required.Field "value" Decode.string }

    let call (term: Decoder<AST.Nodes.Term>) (get: Decode.IGetters) =
        AST.Nodes.Term.Call
            { location = get |> location
              callee = get.Required.Field "callee" term
              arguments = get.Required.Field "arguments" (Decode.array term) }

    let binaryOp =
        Decode.string
        |> Decode.andThen (fun op ->
            match op with
            | "Add" -> Decode.succeed AST.Nodes.BinaryOp.Add
            | "Sub" -> Decode.succeed AST.Nodes.BinaryOp.Sub
            | "Mul" -> Decode.succeed AST.Nodes.BinaryOp.Mul
            | "Div" -> Decode.succeed AST.Nodes.BinaryOp.Div
            | "Rem" -> Decode.succeed AST.Nodes.BinaryOp.Rem
            | "Eq" -> Decode.succeed AST.Nodes.BinaryOp.Eq
            | "Neq" -> Decode.succeed AST.Nodes.BinaryOp.Neq
            | "Lt" -> Decode.succeed AST.Nodes.BinaryOp.Lt
            | "Gt" -> Decode.succeed AST.Nodes.BinaryOp.Gt
            | "Lte" -> Decode.succeed AST.Nodes.BinaryOp.Lte
            | "Gte" -> Decode.succeed AST.Nodes.BinaryOp.Gte
            | "And" -> Decode.succeed AST.Nodes.BinaryOp.And
            | "Or" -> Decode.succeed AST.Nodes.BinaryOp.Or
            | "Not" -> Decode.succeed AST.Nodes.BinaryOp.Not
            | _ -> Decode.fail $"Invalid operation {op}")

    let binary (term: Decoder<AST.Nodes.Term>) (get: Decode.IGetters) =
        AST.Nodes.Term.Binary
            { location = get |> location
              lhs = get.Required.Field "lhs" term
              op = get.Required.Field "op" binaryOp
              rhs = get.Required.Field "rhs" term }

    let param: Decoder<AST.Nodes.Param> =
        Decode.object (fun get ->
            { text = get.Required.Field "text" Decode.string
              location = get |> location })

    let func (term: Decoder<AST.Nodes.Term>) (get: Decode.IGetters) =
        AST.Nodes.Term.Function
            { location = get |> location
              parameters = get.Required.Field "parameters" (Decode.array param)
              value = get.Required.Field "value" term }

    let varField: Decoder<AST.Nodes.Var> =
        Decode.object (fun get ->
            { location = get |> location
              text = get.Required.Field "text" Decode.string })

    let varTerm (get: Decode.IGetters) =
        AST.Nodes.Term.Var
            { location = get |> location
              text = get.Required.Field "text" Decode.string }

    let ``let`` (term: Decoder<AST.Nodes.Term>) (get: Decode.IGetters) =
        AST.Nodes.Term.Let
            { location = get |> location
              name = get.Required.Field "name" varField
              value = get.Required.Field "value" term
              next = get.Required.Field "next" term }

    let ``if`` (term: Decoder<AST.Nodes.Term>) (get: Decode.IGetters) =
        AST.Nodes.Term.If
            { location = get |> location
              condition = get.Required.Field "condition" term
              ``then`` = get.Required.Field "then" term
              otherwise = get.Required.Field "otherwise" term }

    let print (term: Decoder<AST.Nodes.Term>) (get: Decode.IGetters) =
        AST.Nodes.Term.Print
            { location = get |> location
              value = get.Required.Field "value" term }

    let first (term: Decoder<AST.Nodes.Term>) (get: Decode.IGetters) =
        AST.Nodes.Term.First
            { location = get |> location
              value = get.Required.Field "value" term }

    let second (term: Decoder<AST.Nodes.Term>) (get: Decode.IGetters) =
        AST.Nodes.Term.Second
            { location = get |> location
              value = get.Required.Field "value" term }

    let boolean (get: Decode.IGetters) =
        AST.Nodes.Term.Bool
            { location = get |> location
              value = get.Required.Field "value" Decode.bool }

    let tuple (term: Decoder<AST.Nodes.Term>) (get: Decode.IGetters) =
        AST.Nodes.Term.Tuple
            { location = get |> location
              first = get.Required.Field "first" term
              second = get.Required.Field "second" term }

    let expression: Decoder<AST.Nodes.Term> =
        let rec exp () =
            Decode.object (fun get ->
                let kind = get.Required.Field "kind" Decode.string

                match kind with
                | "Int" -> get |> integer
                | "Str" -> get |> string
                | "Call" -> get |> call (exp ())
                | "Binary" -> get |> binary (exp ())
                | "Function" -> get |> func (exp ())
                | "Let" -> get |> ``let`` (exp ())
                | "If" -> get |> ``if`` (exp ())
                | "Print" -> get |> print (exp ())
                | "First" -> get |> first (exp ())
                | "Second" -> get |> second (exp ())
                | "Bool" -> get |> boolean
                | "Tuple" -> get |> tuple (exp ())
                | "Var" -> get |> varTerm
                | _ -> failwith $"Invalid Expression kind: {kind}")

        exp ()

    let file: Decoder<AST.Nodes.File> =
        Decode.object (fun get ->
            { name = get.Required.Field "name" Decode.string
              location = get |> location
              expression = get.Required.Field "expression" expression })

let parse = Decode.fromString Decode.file
