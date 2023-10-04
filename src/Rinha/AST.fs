module Rinha.AST

open System.Numerics

module Nodes =
   type File = {
        name: string
        expression: Term
        location: Loc
   }
    and Loc = {
        start: int
        ``end``: int
        filename: string
    }
    and Literal<'T> = {
        value: 'T
        location: Loc
    }
    and If = {
        condition: Term
        ``then``: Term
        otherwise: Term
        location: Loc
    }
    and Let = {
        name: Var
        value: Term
        next: Term
        location: Loc
    }
    and Bool = Literal<bool>
    and Int = Literal<BigInteger>
    and Str = Literal<string>
    and Var = {
        text: string
        location: Loc
    }
    and Binary = {
        lhs: Term
        op: BinaryOp
        rhs: Term
        location: Loc
    }
    and BinaryOp =
        | Add
        | Sub
        | Mul
        | Div
        | Rem
        | Eq
        | Neq
        | Lt
        | Gt
        | Lte
        | Gte
        | And
        | Or
        | Not
    and Call = {
        callee: Term
        arguments: Term[]
        location: Loc        
    }
    and Function = {
        parameters: Param[]
        value: Term
        location: Loc
    }
    and Print = Literal<Term>
    and First = Literal<Term>
    and Second = Literal<Term>
    and Tuple = {
        first: Term
        second: Term
        location: Loc
    }
    and Param = {
        text: string
        location: Loc
    }
    and Term =
        | Int of Int
        | Str of Str
        | Call of Call
        | Binary of Binary
        | Function of Function
        | Let of Let
        | If of If
        | Print of Print
        | First of First
        | Second of Second
        | Bool of Bool
        | Tuple of Tuple
        | Var of Var
        | Null
        
    module Term =
        let location = function
            | Int t -> t.location
            | Str t -> t.location
            | Call t -> t.location
            | Binary t -> t.location
            | Function t -> t.location
            | Let t -> t.location
            | If t -> t.location
            | Print t -> t.location
            | First t -> t.location
            | Second t -> t.location
            | Bool t -> t.location
            | Tuple t -> t.location
            | Var t -> t.location
            | Null -> { start  = -1; ``end`` = -1; filename = "null" }