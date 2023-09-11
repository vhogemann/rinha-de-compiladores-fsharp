module Rinha.Interpreter.Binary

open Rinha.AST.Nodes
open Rinha.Interpreter

module Str =
    let apply op (lhs:Str) (rhs:Str) (term:Term) : Result<Term,RuntimeError> =
        let loc = term |> Term.location
        match op with
        | Add -> Str { value = $"{lhs.value}{rhs.value}"; location = loc  } |> Ok 
        | Eq  -> Bool { value = (lhs.value = rhs.value); location = loc} |> Ok
        | Neq -> Bool { value = (lhs.value <> rhs.value); location = loc} |> Ok
        | op -> Error { description = $"{op} Unsupported for type Str"; location = term }
        
    let convert (term:Term) : Result<Str,RuntimeError> =
        match term with
        | Str str -> str |> Ok
        | Bool b -> Ok { value = $"{b.value}"; location=b.location }
        | Int i -> Ok { value = $"{i.value}"; location=i.location }
        | op -> Error { description = $"Unsupported conversion from {op} to Str"; location = op }
    
    let execute op lhs rhs term =
        convert lhs
        |> Result.bind( fun lhs ->
            convert rhs
            |> Result.bind (fun rhs -> apply op lhs rhs term))

module Int =
    let execute op (lhs:Int) (rhs:Int) term =
        let loc = term |> Term.location
        match op with
        | Add ->
            Int { value = lhs.value + rhs.value; location = loc } |> Ok
        | Sub ->
            Int { value = lhs.value - rhs.value; location = loc } |> Ok
        | Mul ->
            Int { value = lhs.value * rhs.value; location = loc } |> Ok
        | Div ->
            if rhs.value = 0M then
                Error { description  = "Division by ZERO"; location = term }
            else
                Int { value = lhs.value / rhs.value; location = loc } |> Ok
        | Rem ->
            Int { value = lhs.value % rhs.value; location = loc } |> Ok
        | Eq ->
            Bool { value = lhs.value = rhs.value; location = loc } |> Ok
        | Neq ->
            Bool { value = lhs.value <> rhs.value; location = loc } |> Ok
        | Lt ->
            Bool { value = lhs.value < rhs.value; location = loc } |> Ok
        | Gt ->
            Bool { value = lhs.value > rhs.value; location = loc } |> Ok
        | Lte ->
            Bool { value = lhs.value <= rhs.value; location = loc } |> Ok
        | Gte ->
            Bool { value = lhs.value >= rhs.value; location = loc } |> Ok
        | _ ->
            Error { description = $"{op} Unsupported for type Int"; location = term }


module Bool =
    let execute op (lhs:Bool) (rhs:Bool) term =
        let loc = term |> Term.location
        match op with
        | Eq ->
            Bool { value = lhs.value = rhs.value; location = loc } |> Ok
        | Neq ->
            Bool { value = lhs.value <> rhs.value; location = loc } |> Ok
        | And ->
            Bool { value = lhs.value && rhs.value; location = loc } |> Ok
        | Or ->
            Bool { value = lhs.value || rhs.value; location = loc } |> Ok
        | Not ->
            Bool { value = not rhs.value; location = loc } |> Ok //???
        | _ ->
             Error { description = $"{op} Unsupported for type Bool"; location = term }


let isLiteral = function
    | Term.Bool _
    | Term.Int _
    | Term.Str _ -> true
    | _ -> false
    
let execute (lhs:Term) (op:BinaryOp) (rhs:Term) term =
     match lhs, rhs with
     | Str _, _
     | _, Str _ -> Str.execute op lhs rhs term
     | Int l, Int r ->
         Int.execute op l r term
     | Bool l, Bool r ->
         Bool.execute op l r term
     | l, r -> Error { description = $"Unsupported operation ${op} for terms ${l} and ${r}"; location = term } 
    
let eval (evaluator:Eval) (term:Binary) (context:Context) =
    let lhs = evaluator term.lhs context
    let rhs = evaluator term.rhs context
    match lhs.result, rhs.result with
    | Error _, _ ->
        lhs
    | _, Error _ ->
        rhs
    | Ok lhs, Ok rhs when (isLiteral lhs) && (isLiteral rhs) ->
        context
        |> Context.withResult (execute lhs (term.op) rhs (Binary term))
    | _ ->
        let error = { description = $"Expecting Literal/Literal, got {lhs.result}/{lhs.result} "; location = Binary term }
        context
        |> Context.withResult (Error error)

