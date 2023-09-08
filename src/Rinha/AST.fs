module Rinha.AST

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
    and If = {
        kind: string
        condition: Term
        ``then``: Term
        otherwise: Term
        location: Loc
    }
    and Let = {
        kind: string
        name: Var
        value: Term
        next: Term
        location: Loc
    }
    and Bool = {
        kind: string
        value: bool
        location: Loc
    }
    and Int = {
        kind: string
        value: decimal
        location: Loc
    }
    and Str = {
        kind: string
        value: string
        location: Loc
    }
    and Var = {
        kind: string
        text: string
        location: Loc
    }
    and Binary = {
        kind: string
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
        kind: string
        callee: Term
        arguments: Term[]
        location: Loc        
    }
    and Function = {
        kind: string
        parameters: Param[]
        value: Term
        location: Loc
    }
    and Print = {
        kind: string
        value: Term
        location: Loc
    }
    and First = {
        kind: string
        value: Term
        location: Loc
    }
    and Second = {
        kind: string
        value: Term
        location: Loc
    }
    and Tuple = {
        kind: string
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