namespace Rinha.Interpreter

open Rinha.AST.Nodes

type RuntimeError = {
    description: string
    location: Term
}

type Context = {
    locals: Map<string,Term>
    result: Result<Term,RuntimeError>
}

type Eval = Term -> Context -> Context

module Context =
    let empty:Context = {
        locals = Map.empty
        result = Term.Null |> Ok
    }
    
    let declare varName varValue context =
        { context with
            locals =
                context.locals
                |> Map.add varName varValue
        }
    
    let literal term =
        { empty with
            result = Ok term           
        }
    let result context =
        context.result
        
    let withResult result context =
        { context with result = result }

module Var =
    let eval (term:Var) (context:Context) =
        { Context.empty with result = context.locals.[term.text] |> Ok }

module Let =
    let eval (evaluator:Eval) (term:Let) (context:Context) =
        context
        |> Context.declare term.name.text term.value
        |> evaluator term.next

module Print =
    let eval (evaluator:Eval) (term:Print) (context:Context) =
        let context = context |> evaluator term.value
        context    

