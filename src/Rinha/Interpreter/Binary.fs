module Rinha.Interpreter.Binary

open Rinha.AST.Nodes
open Rinha.Interpreter

module Str =
    let apply (lhs:Str) (rhs:Str) (term:Binary) : Result<Term,RuntimeError> =
        let loc = term.location
        match term.op with
        | Add -> Str { value = $"{lhs.value}{rhs.value}"; location = loc  } |> Ok 
        | Eq  -> Bool { value = (lhs.value = rhs.value); location = loc} |> Ok
        | Neq -> Bool { value = (lhs.value <> rhs.value); location = loc} |> Ok
        | op -> Error { description = $"{op} Unsupported for type Str"; location = Binary term }
        
    let convert (term:Term) : Result<Str,RuntimeError> =
        match term with
        | Str str -> str |> Ok
        | Bool b -> Ok { value = $"{b.value}"; location=b.location }
        | Int i -> Ok { value = $"{i.value}"; location=i.location }
        | op -> Error { description = $"Unsupported conversion from {op} to Str"; location = op }
    
    let execute lhs rhs term =
        convert lhs
        |> Result.bind( fun lhs ->
            convert rhs
            |> Result.bind (fun rhs -> apply lhs rhs term))

module Int =
    let execute (lhs:Int) (rhs:Int) (term:Binary) =
        let loc = term.location
        match term.op with
        | Add ->
            Int { value = lhs.value + rhs.value; location = loc } |> Ok
        | Sub ->
            Int { value = lhs.value - rhs.value; location = loc } |> Ok
        | Mul ->
            Int { value = lhs.value * rhs.value; location = loc } |> Ok
        | Div ->
            if rhs.value = 0M then
                Error { description  = "Division by ZERO"; location = Binary term }
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
        | op ->
            Error { description = $"{op} Unsupported for type Int"; location = Binary term }


module Bool =
    let execute (lhs:Bool) (rhs:Bool) (term:Binary) =
        let loc = term.location
        match term.op with
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
             Error { description = $"{term.op} Unsupported for type Bool"; location = Binary term }


let isLiteral = function
    | Term.Bool _
    | Term.Int _
    | Term.Str _ -> true
    | _ -> false
    
let execute (lhs:Term) (rhs:Term) binary =
     match lhs, rhs with
     | Str _, _
     | _, Str _ -> Str.execute lhs rhs binary
     | Int l, Int r ->
         Int.execute l r binary
     | Bool l, Bool r ->
         Bool.execute l r binary
     | l, r -> Error { description = $"Unsupported operation ${binary.op} for terms ${l} and ${r}"; location = Binary binary }