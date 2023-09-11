module Rinha.Interpreter

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
    let eval (term:AST.Nodes.Var) (context:Context) =
        { Context.empty with result = context.locals.[term.text] |> Ok }

module Let =
    let eval (evaluator:Eval) (term:AST.Nodes.Let) (context:Context) =
        context
        |> Context.declare term.name.text term.value
        |> evaluator term.next

module Print =
    let eval (evaluator:Eval) (term:AST.Nodes.Print) (context:Context) =
        let context = context |> evaluator term.value
        context    

module Call =
    let isFunction (term:Term) =
        match term with
        | Term.Function fn -> Ok fn
        | _ -> Error { description = "Expected a function"; location = term }
    
    let resolve (arguments:Result<Term,RuntimeError>[]) =
        arguments
        |> Array.fold (fun state args ->
            match args with
            | Ok argument -> fst state |> Array.append [| argument |], snd state
            | Error error -> fst state, snd state |> Array.append [| error |]
            ) ([||],[||])
        |> fun (args,errors) ->
            if errors |> Array.isEmpty then
                Ok args
            else
                Error errors.[0]
                
    let execute (evaluator:Eval) (fn:Function) (call:Call) (context:Context) =
        let paramLenght = fn.parameters |> Array.length
        let argLenght = call.arguments |> Array.length
        if paramLenght <> argLenght then
            Error { description = $"Expected %d{paramLenght} arguments but got %d{argLenght}"
                    location = Term.Null }
        else
            let paramNames = fn.parameters |> Array.map (fun p -> p.text)
            
            call.arguments
            |> Array.map (fun arg -> evaluator arg context)
            |> Array.map Context.result
            |> resolve
            |> Result.map (fun args ->
                let locals =
                    context.locals
                    |> Map.toArray
                    |> Array.append (args |> Array.zip paramNames)
                    |> Map.ofArray
                { context with locals = locals }
            )
            |> Result.bind (fun context ->
                context
                |> evaluator fn.value
                |> Context.result
            )

    let eval (evaluator:Eval) (term:AST.Nodes.Call) (context:Context) =
        let result =
            context |> evaluator term.callee
            |> Context.result
            |> Result.bind isFunction
            |> Result.bind (fun fn -> execute evaluator fn term context)
        context |> Context.withResult result

module Binary =
    
    let isLiteral = function
        | Term.Bool _
        | Term.Int _
        | Term.Str _ -> true
        | _ -> false
    
    let eval (evaluator:Eval) (term:AST.Nodes.Binary) (context:Context) =
        let lhs = evaluator term.lhs context
        let rhs = evaluator term.rhs context
        match lhs.result, rhs.result with
        | Ok lhs, Ok rhs when (isLiteral lhs) && (isLiteral rhs) ->
        
        
        ()
        
module Term =
    let rec eval (term:Term) (context:Context) : Context =
        match term with
        | Term.Var node ->
            Var.eval node context
        | Term.Let node ->
            Let.eval eval node context
        | Term.Call call ->
            Call.eval eval call context
        | Term.Binary binary -> failwith "todo"
        | Term.If ``if`` -> failwith "todo"
        | Term.Print node ->
            Print.eval eval node context
        | Term.First first -> failwith "todo"
        | Term.Second second -> failwith "todo"
        | Term.Null -> failwith "todo"
        | Term.Bool _
        | Term.Int _
        | Term.Str _
        | Term.Tuple _
        | Term.Function _ ->
            Context.literal term
