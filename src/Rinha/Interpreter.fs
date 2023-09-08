module Rinha.Interpreter

open Rinha.AST.Nodes

type Context = {
    locals: Map<string,AST.Nodes.Term>
    result: AST.Nodes.Term
}

type Eval = AST.Nodes.Term -> Context -> Context

module Context =
    let empty:Context = {
        locals = Map.empty
        result = AST.Nodes.Term.Null
    }
    
    let declare varName varValue context =
        { context with
            locals =
                context.locals
                |> Map.add varName varValue
        }
    
    let literal term =
        { empty with
            result = term            
        }

module Var =
    let eval (term:AST.Nodes.Var) (context:Context) =
        { Context.empty with result = context.locals.[term.text] }

module Let =
    let eval (evaluator:Eval) (term:AST.Nodes.Let) (context:Context) =
        context
        |> Context.declare term.name.text term.value
        |> evaluator term.next

module Term =
    let rec eval (term:AST.Nodes.Term) (context:Context) : Context =
        match term with
        | Term.Var node ->
            Var.eval node context
        | Term.Let node ->
            Let.eval eval node context
        | Term.Call call -> failwith "todo"
        | Term.Binary binary -> failwith "todo"
        | Term.If ``if`` -> failwith "todo"
        | Term.Print print -> failwith "todo"
        | Term.First first -> failwith "todo"
        | Term.Second second -> failwith "todo"
        | Term.Null -> failwith "todo"
        | Term.Bool _
        | Term.Int _
        | Term.Str _
        | Term.Tuple _
        | Term.Function _ ->
            Context.literal term
        