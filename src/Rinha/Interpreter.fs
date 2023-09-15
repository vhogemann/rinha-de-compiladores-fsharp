module Rinha.Interpreter

open Rinha.AST.Nodes

type Lit =
| Int of decimal
| Str of string
| Bool of bool
| Error of string

type Evaluator = Term -> Lit

module Literal =
    let visitStr (str:Str): Lit =
        str.value |> Lit.Str
        
    let visitInt (int:Int): Lit =
        int.value |> Lit.Int
    
    let visitBool (bool:Bool): Lit =
        bool.value |> Lit.Bool

module Binary =
    
    let visitStr left right binary =
        match binary.op with
        | BinaryOp.Add -> Lit.Str (left + right)
        | BinaryOp.Eq -> Lit.Bool (left = right)
        | _ -> Lit.Error $"Unsupported operation {binary.op} for type Str @ %A{binary.location}"
    let visit (eval:Evaluator) (bin:Binary): Lit =
        let left = bin.lhs |> eval
        let right = bin.rhs |> eval
        match left, right with
        | Lit.Str _, _
        | Lit.Int _, Lit.Int _
        | Lit.Bool _, Lit.Bool _
        | _, _ ->
            Lit.Error $"Unsupported operation {bin.op} for types {left.GetType().Name} and {right.GetType().Name} @ %A{bin.location}"