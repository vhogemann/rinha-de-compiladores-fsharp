namespace Rinha.Interpreter

open System.Text
open Rinha.AST.Nodes

type RuntimeError = {
    description: string
    location: Term
}

type Context = {
    locals: Map<string,Term>
    output: StringBuilder
    result: Result<Term,RuntimeError>
}

type Eval<'T> = 'T -> Context -> Context

module Context =
    let empty:Context = {
        locals = Map.empty
        output = StringBuilder() 
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
    let eval (context:Context) (term:Var) =
        { Context.empty with result = context.locals.[term.text] |> Ok }

module Let =
    let eval (context:Context) (term:Let) =
        context
        |> Context.declare term.name.text term.value

module Print =
    let eval (evaluator:Eval<Term>) (term:Print) (context:Context) =
        let context = context |> evaluator term.value
        context    

