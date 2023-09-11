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
    and Int = Literal<int64>
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
    and Print = {
        value: Term
        location: Loc
    }
    and First = {
        value: Term
        location: Loc
    }
    and Second = {
        value: Term
        location: Loc
    }
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