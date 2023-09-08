module Rinha.Interpreter

type Context = {
    locals: Map<string,AST.Nodes.Term>
    result: AST.Nodes.Term
}

type Evaluator = AST.Nodes.Term -> Context -> Context

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
    let eval (evaluator:Evaluator) (term:AST.Nodes.Let) (context:Context) =
        context
        |> Context.declare term.name.text term.value
        |> evaluator term.next      

module Term =
    let rec eval (term:AST.Nodes.Term) (context:Context) : Context =
        match term with
        | AST.Nodes.Term.Let node ->
            Let.eval eval node context
        | _ ->
            Context.literal term